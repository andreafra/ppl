-module(ex_2023_12_13).
-compile(export_all).

hello_world() ->
    "Hello world".

greet(Name) when is_list(Name) ->
    "Hello " ++ Name ++ "!";
greet(_) ->
    "Sorry, you need to put a string as input".

is_adult(Age) when Age >= 18 -> true;
is_adult(_) -> false.

greet_adult(Name, Age) ->
    case is_adult(Age) of
        true -> "Hi" ++ Name;
        _ -> "Sorry, too young!"
    end.

how_long([]) -> 0;
how_long([_ | Xs]) -> how_long(Xs) + 1.

%%% Rock, Paper, Scissors

rps_start() ->
    % Pid = spawn(?MODULE, rps_loop, []).
    Pid = spawn(fun() -> rps_loop() end),
    io:format("RPS Pid = ~p~n", [Pid]),
    Pid ! {self(), rock},
    Pid ! {self(), rock},
    Pid ! {self(), rock},
    rps_win_loop().

rps_win_loop() ->
    receive
        win -> io:format("Hooray!");
        Result -> io:format("~p~n", [Result])
    after 5000 -> exit(ok)
    end.
%rps_win_loop().

rps_loop() ->
    io:format("[CPU] Ready!~n"),
    RPS = [rock, paper, scissors],
    CPUChoice = lists:nth(rand:uniform(2), RPS),
    receive
        {PlayerPid, PlayerChoice} ->
            PlayerPid ! ex2_actor_model:rps_win(PlayerChoice, CPUChoice)
    end,
    rps_loop().

%%

bad_proc() ->
    io:format("I'm going to crash!~n"),
    4 / 0.

test_spawn() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> bad_proc() end),
    receive
        Err -> io:format("Received ~p~n", [Err])
    end.

dying_parent(N) ->
    spawn_link(fun() -> child(N) end),
    io:format("[P] I'm gonna die~n"),
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

dying_parent_m() ->
    Pid = self(),
    spawn(fun() -> child_m(Pid) end),
    receive
    after 1000 -> ok
    end,
    io:format("[P] *dies*~n"),
    exit(ok).

child_m(ParentPid) ->
    io:format("[C] Monitoring parent...~n"),
    monitor(process, ParentPid),
    receive
        Any -> io:format("[C] Parent is dead!~n~p~n", [Any])
    after 2000 -> waiting
    end.

foo() ->
    receive
    after 1000 -> io:format("Bar = ~p~n", [var:bar()])
    end,
    foo().

foo_parent() ->
    spawn(fun() -> foo() end).

%%% EXAM
%%% 1. Define a “deep reverse” function, which
%%% takes a “deep” list, i.e. a list containing
%%% possibly lists of any depths, and returns
%%% its reverse.
% deeprev([1,2,[3,[4,5]],[6]]) is [[6],[[5,4],3],2,1].
%%% 2. Define a parallel version of the previous function.

deeprev([X | Xs]) -> deeprev(Xs) ++ [deeprev(X)];
deeprev(X) -> X.

deeprevp(List) ->
    Self = self(),
    dp(Self, List),
    receive
        {Self, Res} -> Res
    end.

dp(Pid, [X | Xs]) ->
    Self = self(),
    PX = spawn(fun() -> dp(Self, X) end),
    PXs = spawn(fun() -> dp(Self, Xs) end),
    receive
        {PX, V} ->
            receive
                {PXs, Vs} -> Pid ! {Self, Vs ++ [V]}
            end
    end;
dp(Pid, X) ->
    Pid ! {self(), X}.
