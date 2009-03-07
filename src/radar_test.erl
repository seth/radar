-module(radar_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

roundtrip_test() ->
    radar_db:init_db(),
    radar:start(),
    S1 = radar:make_service("foo", "g1", "http://foo:123", []),
    S2 = radar:make_service("bar", "g1", "http://bar", []),
    ?assertMatch(ok, radar:register(S1)),
    ?assertMatch(ok, radar:register(S2)),
    [R1|_] = radar:find("foo", "", []),
    [R2|_] = radar:find("bar", "", []),
    ?assertMatch("http://foo:123",radar:service_url(R1)),
    ?assertMatch("http://bar", radar:service_url(R2)),
    ?assertMatch([], radar:find("Not there", "", [])).
