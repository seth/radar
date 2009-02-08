%% @author Seth Falcon <seth@userprimary.net>
%% @copyright 2009 Seth Falcon

%% @doc URL parsing utilities
-module(urlutil).
-author('seth@userprimary.net').
%%-export([parse_url/1]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-import(lists, [map/2]).
-import(string, [join/2, substr/3, substr/2, str/2, chr/2, tokens/2]).

%% @spec parse_url(Url) -> {Scheme, Host, Port, Path, Params} |
%%                         {error, Why::atom(), Url}
%%     Scheme = string(),
%%     Host = string(),
%%     Port = integer(),
%%     Path = string(),
%%     Params = [{Key::string(), Value::string()}]
%%
%% @doc Parses a URL into scheme, host, port, path, and query
%% parameters.  If no port is found, Port will be zero.
parse_url(Url) ->
    case parse_scheme(Url) of
        {ok, Scheme, Rest} ->
            case parse_host(Rest) of
                {error, Why, _} ->
                    {error, Why, Url};
                {Host, Port, PathWithParams} ->
                    {Path, Params} = parse_params(PathWithParams),
                    {Scheme, Host, Port, Path, Params}
            end;
        {error, Why, _} ->
            {error, Why, Url}
    end.

%% @spec make_url(UrlSpec) -> string()
%%    UrlSpec = {Scheme, Host, Port, Path, Params}
%%
%% @doc Converts a parsed URL into a string.  See parse_url/1 for the
%% expected format.
make_url({Scheme, Host, Port, Path, Params}) ->
    PortStr = if
                  Port > 0 -> ":" ++ integer_to_list(Port);
                  true -> ""
              end,
    ParamStr = join_query_params(Params),
    lists:concat([Scheme, "://", Host, PortStr, Path, ParamStr]).

join_query_params([]) ->
    [];
join_query_params(Params) ->
    "?" ++
        join(map(fun({K, V}) -> lists:concat([K, "=", V]) end,
                              Params), "&").

parse_host(Url) ->
    parse_host(Url, [], []).
parse_host([], PortAcc, RestAcc) ->
    {lists:reverse(RestAcc), port_from_acc(PortAcc), []};
parse_host([$/|Rest], PortAcc, Acc) ->
    {lists:reverse(Acc), port_from_acc(PortAcc), [$/|Rest]};
parse_host([$:|[C|Rest]], [], Tsoh)
  when is_integer(C), C > $0, C < $9 ->
    parse_host(Rest, [C], Tsoh);
parse_host([$:|[C|Rest]], [], Tsoh) ->
    {error, bad_port, lists:reverse(Tsoh) ++ ":" ++ [C|Rest]};
parse_host([C|Rest], PortAcc, Tsoh)
  when length(PortAcc) > 0, is_integer(C), C > $0, C < $9 ->
    parse_host(Rest, [C|PortAcc], Tsoh);
parse_host([C|Rest], PortAcc, Tsoh)
  when length(PortAcc) > 0, is_integer(C) ->
    {error, bad_port, lists:reverse(Tsoh) ++ ":" ++ [C|Rest]};
parse_host([C|Rest], PortAcc, Acc) when length(PortAcc) =:= 0 ->
    parse_host(Rest, PortAcc, [C|Acc]).

parse_scheme(Url) ->
    Pos = str(Url, "://"),
    if Pos > 1 ->
            {ok, substr(Url, 1, Pos - 1), substr(Url, Pos + 3)};  
       true ->
            {error, no_scheme, Url}
    end.

parse_params(Url) ->
    Pos = chr(Url, $?),
    if Pos > 0 ->
            Params = map(fun([K|[$=|V]]) -> {[K], V} end,
                               tokens(substr(Url, Pos + 1), "&")),
            {substr(Url, 1, Pos - 1), Params};
       true -> {Url, []}
    end.

port_from_acc(PortAcc) ->
    if
        length(PortAcc) > 0 ->
            list_to_integer(lists:reverse(PortAcc));
        true -> 0
    end.

parse_scheme_test_() ->
    [
     ?_assertMatch({ok, "http", "foo.bar"}, parse_scheme("http://foo.bar")),
     ?_assertMatch({ok, "http", ""}, parse_scheme("http://")),
     ?_assertMatch({error, no_scheme, _}, parse_scheme("blah")),
     ?_assertMatch({error, no_scheme, _}, parse_scheme("://blah"))
    ].

parse_host_test_() ->
    [
     ?_assertMatch({"foo.bar.com",0,[]}, parse_host("foo.bar.com")),
     ?_assertMatch({"foo.bar.com",123,[]}, parse_host("foo.bar.com:123")),
     ?_assertMatch({"foo.bar.com",0,"/a/b"}, parse_host("foo.bar.com/a/b")),
     ?_assertMatch({"f.com",4,"/a/b"}, parse_host("f.com:4/a/b")),
     ?_assertMatch({error,bad_port,"f.com:x"}, parse_host("f.com:x")),
     ?_assertMatch({error,bad_port,"f.com:y2/"}, parse_host("f.com:1y2/"))
    ].


