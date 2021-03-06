-module(eldap_client).

-export([start/0, start/1, start/4, log/3]).

start() ->
    start("localhost", 636, "CN=lbakken,OU=users,DC=bakken,DC=io", "test1234").

start([Host, Port, Dn, Passwd]) when is_atom(Host) andalso is_atom(Port)
                                     andalso is_atom(Dn) andalso is_atom(Passwd) ->
    HostStr = atom_to_list(Host),
    PortStr = binary_to_integer(atom_to_binary(Port)),
    DnStr = atom_to_list(Dn),
    PasswdStr = atom_to_list(Passwd),
    start(HostStr, PortStr, DnStr, PasswdStr).

start(Host, Port, Dn, Passwd) when is_integer(Port) ->
    ok = io:format("[INFO] Host: ~s~n", [Host]),
    ok = io:format("[INFO] Port: ~p~n", [Port]),
    ok = io:format("[INFO] Dn: ~s~n", [Dn]),
    ok = io:format("[INFO] Passwd: ~s~n", [Passwd]),
    inets:start(),
    ssl:start(),
    SslOpts = [
        {cacertfile, "./certs/ca_certificate.pem"},
        {certfile, "./certs/client_bakkenl-z01_certificate.pem"},
        {keyfile, "./certs/client_bakkenl-z01_key.pem"},
        {reuseaddr, false},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true}
    ],
    Opts = [
        {log, fun ?MODULE:log/3},
        {port, Port},
        {ssl, true},
        {sslopts, SslOpts}
    ],
    case eldap:open([Host], Opts) of
        {ok, Handle} ->
            ok = io:format("[INFO] connection opened to ~s~n", [Host]),
            case eldap:simple_bind(Handle, Dn, Passwd) of
                ok ->
                    ok = io:format("[INFO] simple bind succeeded, DN: ~p~n", [Dn]);
                {ok, Referrals} ->
                    ok = io:format("[INFO] simple bind succeeded, DN: ~p, Referrals: ~p~n", [Dn, Referrals]);
                Error ->
                    ok = io:format("[ERROR] simple bind failed, Error: ~p~n", [Error])
            end,
            ok = io:format("[INFO] closing LDAP connection...~n", []),
            ok = eldap:close(Handle),
            ok = io:format("[INFO] LDAP connection is closed.~n", []);
        Error ->
            ok = io:format("[ERROR] connect failed, Error: ~p~n", [Error])
    end,
    ok = io:format("[INFO] closing LDAP connection...~n", []),
    ok = eldap:close(Handle),
    ok = io:format("[INFO] LDAP connection is closed.~n", []),
    init:stop().

log(Level, FormatString, FormatArgs) ->
    LdapMsg = io_lib:format(FormatString, FormatArgs),
    ok = io:format("[~s] ~p~n", [Level, LdapMsg]).
