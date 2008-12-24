-module(radar_test).
-compile(export_all).

test() ->
    radar:start(),
    ok = radar:register({"Foo", "http://foo"}),
    ok = radar:register({"Bar", "http://bar"}),
    {"Foo", "http://foo"} = radar:find("Foo"),
    {"Bar", "http://bar"} = radar:find("Bar"),
    false = radar:find("Not there"),
    radar:stop(),
    io:format("ok~n"),
    ok.


