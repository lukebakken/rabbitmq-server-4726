-module(tls_client).

-export([start/0, start/1]).

start() ->
    start(9999).

start([Port]) when is_atom(Port) ->
    start(binary_to_integer(atom_to_binary(Port)));
start(Port) when is_integer(Port) ->
    inets:start(),
    ssl:start(),
    SslOpts = [
        {cacertfile, "./certs/ca_certificate.pem"},
        {certfile, "./certs/client_localhost_certificate.pem"},
        {keyfile, "./certs/client_localhost_key.pem"},
        {reuseaddr, false},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true}
    ],
    ok = io:format("[INFO] before ssl:connect~n", []),
    {ok, TlsSocket} = ssl:connect("localhost", Port, SslOpts),
    ok = io:format("[INFO] after ssl:connect~n", []),
    loop(TlsSocket, 0).

loop(TlsSocket, Idx) ->
    Msg = io_lib:format("HELLO ~p~n", [Idx]),
    ok = io:format("[INFO] sending Msg: ~p~n", [Msg]),
    ok = ssl:send(TlsSocket, Msg),
    receive
        {ssl_closed, TlsSocket} ->
            ok = io:format("[INFO] shutdown/close socket~n", []),
            ssl:shutdown(TlsSocket, read_write),
            ssl:close(TlsSocket),
            ok;
        Data ->
            ok = io:format("[INFO] Data: ~p~n", [Data]),
            loop(TlsSocket, Idx)
    after 5000 ->
        loop(TlsSocket, Idx + 1)
    end.
