-module(ex2_actor_model).
-compile(export_all).

rps_start() ->
    % Pid = spawn(?MODULE, rps_loop, []),
    Pid = spawn(fun() -> rps_loop() end),
    io:format("RPS Pid = ~p~n", [Pid]),
    % {This proc Pid, Choice}
    Pid ! {self(), rock},
    % then wait for the answer
    receive
        Result -> Result
    end.

rps_loop() ->
    RPS = [rock, paper, scissor],
    CPUChoice = lists:nth(rand:uniform(2), RPS),
    receive
        {PlayerPid, PlayerChoice} ->
            % how do we send back the result? we need parent Pid (PlayerPid)
            % Res = rps_win(PlayerChoice, CPUChoice)
            PlayerPid ! rps_win(PlayerChoice, CPUChoice)
    end,
    rps_loop().

% Determines who wins the game
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

%%%%%%%%%%%%%%
%%% Generally using spawn() is not a super great idea.
%%% You'd want to link the child process to the parent
%%% after creating it, but the operation is not ATOMIC.
%%% You're better off using `spawn_link` which does it
%%% in one single atomic step. Links are BIDIRECTIONAL.
%%%
%%% Child processes terminate if the parent crashes,
%%% but only if you link them.

bad_proc() ->
    io:format("I am a child process and I'm gonna crash.~n"),
    4 / 0.

test_spawn() ->
    process_flag(trap_exit, true),
    Pid = spawn(fun() -> bad_proc() end),
    % Try to receive it... nothing
    % what if we use link(Pid)?
    % link(Pid),
    % ANSWER: behaves the same as test_spawn_link/0
    receive
        Err -> io:format("Received message from child: ~p~n", [Err])
    end.

test_spawn_link() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> bad_proc() end),
    receive
        Err -> io:format("Received message from child: ~p~n", [Err])
    end.

%% Test that child lives if parent dies
dying_parent(N) ->
    spawn(fun() -> child(N) end),
    io:format("[P] I'm gonna die soon...~n"),
    receive
    after 3000 ->
        io:format("[P] *dead*~n"),
        exit(ok)
    end.

% N is the times child is gonna print, then dies
child(0) ->
    exit(ok);
child(N) ->
    receive
    after 1000 -> io:format("[C] Hey!~n")
    end,
    child(N - 1).

%%% Extra: MONITOR
%%% Monitor lets you know when a process goes down
%%% Usually it's useful when your program can work
%%% even if that process crashes
dying_parent_m() ->
    Pid = self(),
    spawn(fun() -> child_m(Pid) end),
    receive
    after 1000 -> ok
    end,
    io:format("[P] *dies*~n"),
    exit(died).

child_m(ParentPid) ->
    io:format("[C|~p] Monitoring parent (~p)~n", [self(), ParentPid]),
    monitor(process, ParentPid),
    % `process` is an atom, it's used to specify
    % the monitored entity (process, port...)
    receive
        Any -> io:format("[C] Parent's dead!~n~p~n", [Any])
    after 2000 -> io:format("[C] I'm still waiting...~n")
    end.
