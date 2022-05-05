-module(tls_server).

-export([start/0, start/1, sni_fun/1]).

start() ->
    start(9999).

start([Port]) when is_atom(Port) ->
    start(binary_to_integer(atom_to_binary(Port)));
start(Port) when is_integer(Port) ->
    inets:start(),
    ssl:start(),
    SslOpts = [
        {cacertfile, "./certs/ca_certificate.pem"},
        {certfile, "./certs/server_localhost_certificate.pem"},
        {keyfile, "./certs/server_localhost_key.pem"},
        {reuseaddr, false},
        {sni_fun, fun tls_server:sni_fun/1},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true}
    ],
    ok = io:format("[INFO] before ssl:listen(9999, Opts)~n", []),
    {ok, ListenSocket} = ssl:listen(Port, SslOpts),
    ok = io:format("[INFO] after ssl:listen(9999, Opts)~n", []),
    accept_and_handshake(ListenSocket).

accept_and_handshake(ListenSocket) ->
    ok = io:format("[INFO] before ssl:transport_accept~n", []),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSocket),
    ok = io:format("[INFO] after ssl:transport_accept~n", []),
    ok = io:format("[INFO] before ssl:handshake~n", []),
    Result =
        case ssl:handshake(TLSTransportSocket) of
            {ok, S, Ext} ->
                ok = io:format("[INFO] ssl:handshake Ext: ~p~n", [Ext]),
                {ok, S};
            {ok, _} = Res ->
                Res;
            Error ->
                ok = io:format("[ERROR] ssl:handshake Error: ~p~n", [Error]),
                Error
        end,
    case Result of
        {error, _} ->
            ok = init:stop();
        {ok, TlsSocket} ->
            ok = io:format("[INFO] after ssl:handshake, Socket: ~p~n", [TlsSocket]),
            ok = io:format("[INFO] ssl:handshake sni_hostname: ~p~n", [get_sni_hostname(TlsSocket)]),
            write_keylog(TlsSocket),
            ok = loop(TlsSocket),
            accept_and_handshake(ListenSocket)
    end.

loop(TlsSocket) ->
    ok = io:format("[INFO] top of loop/2~n", []),
    receive
        {ssl_closed, TlsSocket} ->
            ok = io:format("[INFO] shutdown/close socket~n", []),
            ssl:shutdown(TlsSocket, read_write),
            ssl:close(TlsSocket),
            ok;
        Data ->
            ok = io:format("[INFO] Data: ~p~n", [Data]),
            loop(TlsSocket)
    after 5000 ->
        ok = io:format("[INFO] no data in last 5 seconds!~n", []),
        loop(TlsSocket)
    end.

write_keylog(Socket) ->
    ok = io:format("[INFO] writing keylog data to keylog.bin~n", []),
    KeylogItems1 =
        case ssl:connection_information(Socket, [keylog]) of
            {ok, [{keylog, KeylogItems0}]} ->
                KeylogItems0;
            {ok, []} ->
                ok = io:format("[WARNING] no keylog data!~n", []),
                []
        end,
    ok = file:write_file("keylog.bin", [[KeylogItem, $\n] || KeylogItem <- KeylogItems1]),
    ok.

sni_fun(ServerName) ->
    ok = io:format("[INFO] sni_fun ServerName: ~p~n", [ServerName]),
    [].

get_sni_hostname(Socket) ->
    case ssl:connection_information(Socket, [sni_hostname]) of
        {ok, []} ->
            undefined;
        {ok, [{sni_hostname, Hostname}]} ->
            Hostname;
        Error ->
            ok = io:format("[ERROR] ssl:connection_information Error: ~p~n", [Error]),
            undefined
    end.
