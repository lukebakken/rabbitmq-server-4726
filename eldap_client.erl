-module(eldap_client).

-export([start/0, start/1, log/3]).

start() ->
    start("localhost", 9999).

start([Host, Port]) when is_atom(Host) andalso is_atom(Port) ->
    start(atom_to_list(Host), binary_to_integer(atom_to_binary(Port))).

start(Host, Port) when is_integer(Port) ->
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
    Opts = [
        {log, fun log/3},
        {port, Port},
        {ssl, true},
        {sslopts, SslOpts}
    ],
    {ok, Handle} = eldap:open([Host], Opts),
    ok = io:format("[INFO] connection opened to ~s~n", [Host]),
    ok = eldap:close(Handle).

log(Level, FormatString, FormatArgs) ->
    LdapMsg = io_lib:format(FormatString, FormatArgs),
    ok = io:format("[~s] ~p~n", [Level, LdapMsg]).
