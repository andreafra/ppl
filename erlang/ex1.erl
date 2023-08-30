-module(ex1).
-export([
    hello_world/0,
    greet/1,
    greet_adult/2,
    greet_person/1
]).

hello_world() ->
    "Hello World".

% Name must start with an uppercase letter,
% because it's a VARIABLE
greet(Name) when is_list(Name) ->
    "Hello " ++ Name ++ "!";
greet(_) -> "Invalid name".

greet_adult(Name, Age) when Age >= 18 ->
    % ~s prints Name as STRING, ~n is the NEWLINE char.
    io:fwrite("Hello ~s~n", [Name]);
greet_adult(Name, _) ->
    io:fwrite("Sorry ~s, you are too young!~n", [Name]).

% A Person (which is a map) has:
% - name
% - age
% - likes
greet_person(#{name := Name, age := Age, likes := Likes}) ->
    Greeting =
        if Age >= 18 ->
            case lists:member(soccer, Likes) of
                true -> "Hello " ++ Name ++ ", I like soccer, too!";
                false -> "Hello " ++ Name
            end
        ; Age < 18 -> "Sorry, too young!"
    end,
    Greeting.
