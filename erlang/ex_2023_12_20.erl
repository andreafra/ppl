-module(ex_2023_12_20).
-compile(export_all).

%%% 2020-02-07
%%% We want to create a simplified implementation of the “Reduce” part of the MapReduce paradigm. To this end, define a process “reduce_manager” that keeps track of a pool of reducers. When it is created, it stores a user-defined associative binary function ReduceF. It receives messages of the form {reduce, Key, Value}, and forwards them to a different “reducer” process for each key, which is created lazily (i.e. only when needed). Each reducer serves requests for a unique key.
% Reducers keep into an accumulator variable the result of the application of ReduceF to the values they receive. When they receive a new value, they apply ReduceF to the accumulator and the new value, updating the former. When the reduce_manager receives the message print_results, it makes all its reducers print their key and incremental result.

% - reduce_mgr
% - reducer
% ReduceF

start_reduce_mgr(ReduceF) ->
    spawn(fun() -> reduce_manager(ReduceF) end).

reduce_manager(ReduceF) -> reduce_manager(ReduceF, #{}).
reduce_manager(ReduceF, Reducers) ->
    receive
        {reduce, Key, Value} ->
            case Reducers of
                #{Key := Pid} ->
                    Pid ! {reduce, Key, Value},
                    reduce_manager(ReduceF, Reducers);
                _ ->
                    NewPid = spawn(fun() -> reducer(Key, ReduceF, 0) end),
                    NewPid ! {reduce, Key, Value},
                    reduce_manager(ReduceF, Reducers#{Key => NewPid})
            end;
        print_results ->
            [Pid ! print_results || {_, Pid} <- maps:to_list(Reducers)],
            reduce_manager(ReduceF, Reducers)
    end.

reducer(Key, ReduceF, Acc) ->
    receive
        {reduce, Key, Value} ->
            reducer(Key, ReduceF, ReduceF(Acc, Value));
        print_results ->
            io:format("~p~n", [Acc]),
            reducer(Key, ReduceF, Acc)
    end.

% TEST
word_count(Text) ->
    RMPid = start_reduce_mgr(fun(X, Y) -> X + Y end),
    lists:foreach(fun(Word) -> RMPid ! {reduce, Word, 1} end, string:split(Text, " ", all)),
    RMPid ! print_results,
    ok.

%> ex...:word_count("sopra la panca la capra campa sotto la panca la capra crepa").


%%% 2020-07-17
% Define a "broadcaster" process which answers to the following
% commands:
% - {spawn, L, V} creates a process for each element of L, passing its
% initial parameter in V, where L is a list of names of functions
% defined in the current module and V is their respective parameters (of
% course it must be |L| = |V|);
% - {send, V}, with V a list of values, sends to each respective process
% created with the previous spawn command a message in V; e.g. {spawn,
% [1,2,3]} will send 1 to the first process, 2 to the second, and 3 to
% the third;
% - stop is used to end the broadcaster, and to also stop every process
% spawned by it.
