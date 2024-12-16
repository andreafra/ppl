-module(ex).
-compile(export_all).
% -export([hello_world/]).

hello_world() ->
    "Hello World".

% greet/0
greet() -> "Hello!".

% greet/1
greet(Name) when is_list(Name) ->
    "Hello " ++ Name ++ "!";
greet(_) ->
    "Invalid Name!".

greet(Name, Age) ->
    %io:format("Hello ~p~n", [Name]),
    greet(Name),
    {CurrentYear, _, _} = date(),
    BirthYear = CurrentYear - Age,
    io:format("You were born in ~p~n", [BirthYear]).

greet_person() ->
    M = #{name => "Andrea", age => 26},
    M1 = M#{age := 27},
    #{age := Age, name := Name} = M1,
    {Age, Name}.

greet_person(#{name := Name}) ->
    "Hello " ++ Name.

is_even(N) ->
    if
        N rem 2 =/= 0 -> true;
        true -> false
    end.

is_odd(N) ->
    case N rem 2 of
        1 -> true;
        _ -> false
    end.

how_long([]) -> 0;
how_long([_ | Xs]) -> 1 + how_long(Xs).

% List = [1, 2, 3, 4, 5]
list_plus_2(List) ->
    lists:map(
        fun(X) -> X + 2 end,
        List
    ).

list_plus_2_again(List) ->
    [X + 2 || X <- List].

%%% CONCURRENCY %%%

rps_start() ->
    % Pid = spawn(?MODULE, rps_loop, [])
    Pid = spawn(fun() -> rps_loop() end),
    Pid.

rps_loop() ->
    RPS = [rock, paper, scissor],
    CPUChoice = lists:nth(rand:uniform(2), RPS),
    receive
        {PlayedPid, PlayerChoice} ->
            Result = rps_win(PlayerChoice, CPUChoice),
            % Sends a message
            PlayedPid ! Result,
            rps_loop();
        exit ->
            ok
    end.

rps_win(Player, CPU) ->
    case {Player, CPU} of
        {rock, paper} -> win;
        {rock, scissor} -> lose;
        {paper, rock} -> win;
        {paper, scissor} -> lose;
        {scissor, paper} -> win;
        {scissor, rock} -> lose;
        {_, _} -> draw
    end.

bad_proc() ->
    io:format("[C] I'm gonna crash~n"),
    4 / 0.

test_spawn_link() ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> bad_proc() end),
    receive
        Err -> io:format("Received msg from child ~p~n", [Err])
    end.

dying_parent(N) ->
    spawn(fun() -> child(N) end),
    % with spawn link the child dies too
    % spawn_link(fun() -> child(N) end),
    io:format("[P] I'm dying soon!~n"),
    receive
    after 3000 ->
        io:format("[P] *dies*~n"),
        exit(ok)
    end.

child(0) ->
    exit(ok);
child(N) ->
    receive
    after 1000 -> io:format("[C] Hey!~n")
    end,
    child(N - 1).
