-module(hexutil).

-export([hex_str_to_bytes/1, to_hex_str/1]).
-include_lib("eunit/include/eunit.hrl").

%% @spec hex_str_to_bytes(string()) -> binary()
%% @doc Convert a hex string to a binary.
hex_str_to_bytes(L) ->
    hex_str_to_bytes(L, <<>>).
hex_str_to_bytes([H1, H2 | T]=L, Ans) when length(L) rem 2 =:= 0 ->
    P1 = hex_chr_to_i(H1),
    P2 = hex_chr_to_i(H2),
    Ans1 = <<Ans/binary, P1:4, P2:4>>,
    hex_str_to_bytes(T, Ans1);
hex_str_to_bytes([], Ans) ->
    Ans.

%% this is more concise, but not tail-recursive (I think)
%% to_hex_str2(<<>>) ->
%%     [];
%% to_hex_str2(<<N1:4, N2:4, BinRest/binary>>) ->
%%     [hex_chr(N1), hex_chr(N2) | to_hex_str(BinRest)].

%% @spec to_hex_str(binary()) -> string()
%% @doc Convert a binary to a hex string.
to_hex_str(B) ->
    to_hex_str(B, []).
to_hex_str(<<>>, Ans) ->
    lists:reverse(Ans);
to_hex_str(<<N1:4, N2:4, BinRest/binary>>, Ans) ->
    to_hex_str(BinRest, [hex_chr(N2), hex_chr(N1) | Ans]).

hex_chr(N) when N >= 0, N < 10 ->
    $0 + N;
hex_chr(N) when N >= 10, N < 16 ->
    $a + (N - 10);
hex_chr(N) ->
    {error, {"invalid input", N}}.

hex_chr_to_i(N) when N >= $0, N < $0 + 10 ->
    N - $0;
hex_chr_to_i(N) when N >= $0 + 10, $f >= N ->
    N + 10 - $a.

%% test functions
hex_chr_test_() ->
    [
     ?_assert("3" =:= [hex_chr(3)]),
     ?_assert("a" =:= [hex_chr(10)]),
     ?_assert("f" =:= [hex_chr(15)]),
     ?_assert({error, {"invalid input", 44}} =:= hex_chr(44)),
     ?_assertMatch("0123456789", lists:map(fun hex_chr/1, lists:seq(0, 9))),
     ?_assertMatch("abcdef", lists:map(fun hex_chr/1, lists:seq(10, 15)))
    ].

hex_chr_to_i_test_() ->
    AllHexInts = lists:seq(0, 15),
    [
     ?_assert(3 =:= hex_chr_to_i($3)),
     ?_assert(10 =:= hex_chr_to_i($a)),
     ?_assert(15 =:= hex_chr_to_i($f)),
     ?_assertMatch(AllHexInts, lists:map(fun hex_chr_to_i/1,
                                         "0123456789abcdef"))
    ].

to_hex_str_md5_test_() ->
    ?_assert("b1946ac92492d2347c6235b4d2611184" ==
             to_hex_str(erlang:md5("hello\n"))).

to_hex_str_md5_round_trip_test_() ->
    B = erlang:md5("hello\n"),
    ?_assert(B =:= hex_str_to_bytes(to_hex_str(B))).

