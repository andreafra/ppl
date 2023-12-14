-module(ex3_dyn_code_reload).
-export([
    start/1
]).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).

loop(Tag) ->
    sleep(),
    % is it gonna be 1 or ...?
    Value = ex3b_lib:x(),
    io:format("[~p] Value=~p~n", [Tag, Value]),
    loop(Tag).

sleep() ->
    receive
    after 3000 -> true
    end.
