-module(aoc2).

-export([start/0]).

% Source https://adventofcode.com/2024/day/2

get_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:split(binary_to_list(Data), "\n", all),
    WordLines = [string:split(L, " ", all) || L <- Lines],
    lists:map(fun parse_word_list/1, WordLines).

parse_word_list(Ws) ->
    lists:map(fun parse_word_to_integer/1, Ws).

parse_word_to_integer(W) ->
    {N, _} = string:to_integer(W),
    N.

start() ->
    Pid = self(),
    Seqs = get_input(),
    Procs = [
        spawn(fun() ->
            validate(Pid, S)
        end)
     || S <- Seqs
    ],
    Total = length(Procs),
    collect(0, Total).

validate(Pid, Seq) ->
    R = is_safe(Seq, 0),
    io:format("~p -> ~p~n", [Seq, R]),
    Pid ! R.

% An empty list is safe
is_safe([], _) ->
    true;
% A list of single element is safe too
is_safe([_], _) ->
    true;
is_safe([A, B | Seq], Sign) ->
    Diff = abs(A - B),
    NewSign = sign(A - B),
    if
        Diff > 3 -> false;
        Diff < 1 -> false;
        Sign =/= 0 andalso Sign =/= NewSign -> false;
        true -> is_safe([B | Seq], NewSign)
    end.

sign(N) when N > 0 -> 1;
sign(N) when N < 0 -> -1;
sign(0) -> 0.

collect(Valid, 0) ->
    Valid;
collect(Valid, Total) ->
    receive
        true -> collect(Valid + 1, Total - 1);
        _ -> collect(Valid, Total - 1)
    end.
