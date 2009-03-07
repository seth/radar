-module(radar_db).
-import(lists, [map/2, sort/1]).

%%-compile(export_all).
-export([init_db/0, start/0, stop/0]).
-export([add/1, remove/1, find_all/0, find/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("service.hrl").

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(service, [{disc_copies, [node()]}, 
                                  {attributes, record_info(fields, service)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([service], 20000).

stop() ->
    mnesia:stop().

add(Service) when is_record(Service, service) ->
    F = fun() -> mnesia:write(Service) end,
    mnesia:transaction(F).

remove(Service) when is_record(Service, service) ->
    Oid = {service, Service#service.id},
    F = fun() -> mnesia:delete(Oid) end, 
    mnesia:transaction(F).

find_all() ->
    do(qlc:q([X || X <- mnesia:table(service)])).

find([], [], Attrs) ->
    do(qlc:q([X || X <- mnesia:table(service),
                   matches_attrs(Attrs, X#service.attrs)]));
find(Type, [], Attrs) ->
    do(qlc:q([X || X <- mnesia:table(service),
                  X#service.type =:= Type,
                  matches_attrs(Attrs, X#service.attrs)]));
find([], Group, Attrs) ->
    do(qlc:q([X || X <- mnesia:table(service),
                  X#service.group =:= Group,
                   matches_attrs(Attrs, X#service.attrs)]));
find(Type, Group, Attrs) ->
    do(qlc:q([X || X <- mnesia:table(service),
                  X#service.type =:= Type,
                  X#service.group =:= Group,
                  matches_attrs(Attrs, X#service.attrs)])).

%% private

has_attr({N, V}, Attrs) ->
    lists:member({N, V}, Attrs).

matches_attrs([], _Have) ->
    true;
matches_attrs(Want, Have) ->
    Member = map(fun(X) -> has_attr(X, Have) end, Want),
    lists:foldr(fun(X, S) -> (X =:= true) and S end, true, Member).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% tests

add_and_remove_test() ->
    init_db(),
    start(),
    mnesia:clear_table(service),
    Services = [
                radar:make_service("us", "g1", "http://1", []),
                radar:make_service("us", "g1", "http://2", [])
               ],
    [radar_db:add(S) || S <- Services],
    [S1, S2|_] = Services,
    ?assertEqual(2, length(find([], [], []))),
    remove(S1),
    ?assertEqual(1, length(find([], [], []))),
    remove(S2),
    ?assertEqual(0, length(find([], [], []))).
    
add_and_find_test_() ->
    init_db(),
    SortedUrls = fun(L) ->
                         sort(map(fun radar:service_url/1, L)) end,
    start(),
    mnesia:clear_table(service),
    Services = [
                radar:make_service("us", "g1", "http://1", []),
                radar:make_service("us", "g1", "http://2", []),
                radar:make_service("us", "g2", "http://3", []),
                radar:make_service("is", "g2", "http://4", []),
                radar:make_service("is", "g3", "http://5", [{"foo", "bar"}])
               ],
    [radar_db:add(S) || S <- Services],
    Tests = [
             {{"", "", []}, ["http://1", "http://2", "http://3",
                             "http://4", "http://5"]},
             {{"none", "", []}, []},
             {{"none", "g1", []}, []},
             {{"", "none", []}, []},
             {{"us", "none", []}, []},
             {{"", "", [{a, b}]}, []},
             {{"us", "g1", []}, ["http://1", "http://2"]},
             {{"us", [], []}, ["http://1", "http://2", "http://3"]},
             {{[], "g3", []}, ["http://5"]},
             {{[], [], [{"foo", "bar"}]}, ["http://5"]},
             {{[], "g3", [{"foo", "bar"}]}, ["http://5"]},
             {{"is", [], [{"foo", "bar"}]}, ["http://5"]},
             {{"is", "g3", [{"foo", "bar"}]}, ["http://5"]}
            ],
    [?_assertMatch(Expect, SortedUrls(find(Type, Group, Attrs)))
     || {{Type, Group, Attrs}, Expect} <- Tests].

matches_attrs_test_() ->
    Tests = [
             {[], [], true},
             {[], [{a, b}, {c, d}], true},
             {[{"a", "b"}], [], false},
             {[{"a", "b"}], [{"c", "d"}], false},
             {[{"a", "b"}], [{"a", "b"}], true},
             {[{"a", "b"}], [{"a", "b"}, {"c", "d"}], true},
             {[{"a", "b"}, {c, d}], [{"a", "b"}, {"c", "d"}], false},
             {[{"a", "b"}, {c, d}], [{"a", "b"}, {c, d}], true}
            ],
    [ ?_assertMatch(Expect, matches_attrs(Want, Have))
      || {Want, Have, Expect} <- Tests ].
