-module(ex4_nodes).

-export([send_msg/1, start_receive_msg/0, say_hello/1]).

% We can connect two separate nodes using
% erl -sname alpha
% erl -sname bravo
%
% run: hostname
% in the the terminal, and get the first part
% of your hostname (for me, the full hostname
% is andreafra.Home, so we just take 'andreafra')
%
% Read more here: https://www.erlang.org/doc/system/distributed.html
%
% Use 'node().' in erl shell to know the
% name of your node.
%
% Generally, if we don't know the Pid of the other
% node, we can use the alternative syntax
% {<process_name>, <node@host>} ! <msg>

% We can therefore exchange messages between processes
% on two or more different, federated machines!

% A function to send a message
send_msg({Node, Msg}) ->
    {reader, Node} ! {self(), Msg}.

% A function to receive a message
start_receive_msg() ->
    % Register this process
    register(reader, self()),
    receive_msg().

receive_msg() ->
    io:format("Waiting for a new message...~n"),
    receive
        {From, Msg} ->
            io:format("Received ~p from~p~n", [Msg, From]),
            receive_msg()
    end.

% We can also spawn a process on another node
% using spawn/4.
% spawn('bravo@andreafra', ex4_nodes, say_hello, ["Bravo"]).
say_hello(Name) ->
    io:format("Hello, ~p from ~p~n", [Name, node()]).
