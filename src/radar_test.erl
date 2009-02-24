-module(radar_test).
-compile(export_all).

test() ->
    radar:start(),
    S1 = radar:make_service("foo", "g1", "http://foo:123", []),
    S2 = radar:make_service("bar", "g1", "http://bar", []),
    ok = radar:register(S1),
    ok = radar:register(S2),
    "http://foo:123" = radar:service_url(radar:find_one("foo")),
    "http://bar" = radar:service_url(radar:find_one("bar")),
    false = radar:find_one("Not there"),
    radar:stop(),
    io:format("ok~n"),
    ok.


