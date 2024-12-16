% 2022.07.06
% We want to implement an interface to a server protocol, for managing requests that are lists of functions of one argument and lists of data on which the functions are called.
% The interface main function is called multiple_query and gets a list of functions FunL, and a list of data, DataL, of course of the same size.
% The protocol works as follows (assume that there is a registered server called master_server):
% 1. First, we ask the master server for a list of slave processes that will perform the computation. The request has the following form:
% {slaves_request, {identity, <id_of_the_caller>}, {quantity, <number_of_needed_slaves>}}
% 2. The answer has the following form:
% {slaves_id, <list_of_ids_of_slave_processes>}
% 3. Then, the library sends the following requests to the slave processes:
% {compute_request, {identity, <id_of_the_caller>}, <function>, <data>},
% where <function> is one of the elements of FunL, and <data> is the corresponding element of DataL.
% 4. Each process sends the result of its computation with a message:
% {compute_result, {identity, <slave_id>}, {value, <result_value>}}
% 5. multiple_query ends by returning the list of the computed results, that must be ordered according to FunL and DataL.
% If you want, you may use lists:zip/2 and lists:zip3/3, that are the standard zip operations on 2 or 3 lists, respectively.

-module(exam1).
-compile(export_all).

multiple_query(FunL, DataL) ->
    % 1
    master_server ! {slaves_request, {identity, self()}, {quantity, length(FunL)}},
    % 2
    receive
        {slaves_id, ProcL} -> ok
    end,
    % 3
    [
        Proc ! {compute_request, {identity, self()}, Fun, Data}
     || {Proc, Fun, Data} <- lists:zip3(ProcL, FunL, DataL)
    ],
    % 5
    [
        % 4
        receive
            {compute_result, {identity, Proc}, {value, Res}} -> Res
        end
     || Proc <- ProcL
    ].

%%%% 2022.02.10
% Define a parallel lexer, which takes as input a string x and a chunk size n, and translates all the words in the strings to atoms, sending to each worker a chunk of x of size n (the last chunk could be shorter than n). You can assume that the words in the string are separated only by space characters (they can be more than one - the ASCII code for ' ' is 32); it is ok also to split words, if they overlap on different chunks.E.g.
% plex("this is a nice test", 6) returns [[this,i],[s,a,ni],[ce,te],[st]]
% For you convenience, you can use the library functions:
% • lists:sublist(List, Position, Size) which returns the sublist of List of size Size from position Position (starting at 1);
% • list_to_atom(Word) which translates the string Word into an atom.

split(Input, Chunks, Pos, End) when Pos < End ->
    [lists:sublist(Input, Pos, Chunks)] ++ split(Input, Chunks, Pos + Chunks, End);
split(_, _, _, _) ->
    [].

% "this i"

% " xxxx"
lex([X | Xs], []) when X =:= 32 ->
    lex(Xs, []);
lex([X | Xs], Word) when X =:= 32 ->
    [list_to_atom(Word)] ++ lex(Xs, []);
lex([X | Xs], WordAccumulating) ->
    lex(Xs, WordAccumulating ++ [X]);
lex([], []) ->
    [];
lex([], Word) ->
    [list_to_atom(Word)].

worker(Pid, Data) ->
    Pid ! {self(), lex(Data, [])}.

plex(Input, Chunks) ->
    End = length(Input),
    % "this is a nice test" -> ["this i", "s a ni", "ce te", ...]
    Parts = split(Input, Chunks, 1, End),
    % "this i" -> worker -> [this,i]
    Pid = self(),
    % [ spawn(?MODULE, worker, [self()]) || P <- Parts ]
    WorkerPids = [spawn(fun() -> worker(Pid, P) end) || P <- Parts],
    lists:map(
        fun(WorkerPid) ->
            receive
                {WorkerPid, Res} -> Res
            end
        end,
        WorkerPids
    ).

%%% 2023.07.03
% 1. Define a “deep reverse” function, which takes a “deep” list, i.e. a list containing possibly lists of any depths, and returns its reverse.
% E.g. deeprev([1,2,[3,[4,5]],[6]]) is [[6],[[5,4],3],2,1].
% 2. Define a parallel version of the previous function.

deeprev([]) -> [];
deeprev([X | Xs]) -> deeprev(Xs) ++ [deeprev(X)];
deeprev(X) -> X.

deeprevp(L) ->
    Pid = self(),
    revhelp(Pid, L),
    receive
        {Pid, R} -> R
    end.

revhelp(Pid, []) ->
    Pid ! {self(), []};
revhelp(Pid, [X | Xs]) ->
    Self = self(),
    PX = spawn(fun() -> revhelp(Self, X) end),
    PXs = spawn(fun() -> revhelp(Self, Xs) end),
    receive
        {PX, RX} ->
            receive
                {PXs, RXs} ->
                    Pid ! {Self, RXs ++ [RX]}
            end
    end;
revhelp(Pid, X) ->
    Pid ! {self(), X}.

%% 2023.01.25

q0() ->
    receive
        {S, [b | Xs]} ->
            q1 ! {S, Xs},
            q2 ! {S, Xs};
        {S, [a | Xs]} ->
            q0 ! {S, Xs}
    end,
    q0().

q1() ->
    receive
        {S, [b | Xs]} -> q0 ! {S, Xs}
    end,
    q1().

q2() ->
    receive
        {S, [b | Xs]} -> q3 ! {S, Xs}
    end,
    q2().

q3() ->
    receive
        {S, [c | Xs]} -> q4 ! {S, Xs}
    end,
    q3().

q4() ->
    receive
        {S, []} -> io:format("string ~p accepted~n", [S])
    end,
    q4().

start() ->
    register(q0, spawn(fun() -> q0() end)),
    register(q1, spawn(fun() -> q1() end)),
    register(q2, spawn(fun() -> q2() end)),
    register(q3, spawn(fun() -> q3() end)),
    register(q4, spawn(fun() -> q4() end)).

% [a, b, a, a, c]
read_strings(S) ->
    q0 ! {S, S},
    ok.
