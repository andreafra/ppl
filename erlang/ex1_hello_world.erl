-module(ex1_hello_world).
-export([
    hello_world/0,
    greet/1,
    greet_adult/2,
    greet_person/1,
    get_how_long/1
]).

hello_world() ->
    "Hello World".

% Name must start with an UPPERCASE letter,
% because it's a VARIABLE
% If its lowercase, it's an ATOM
greet(Name) when is_list(Name) ->
    "Hello " ++ Name ++ "!";
greet(_) ->
    "Invalid name".

% There is also A -- B to subtract list A from list B

% Pattern matching & function evaluation is performed
% in the declaration order
greet_adult(Name, Age) when Age >= 18 ->
    % 'when' is a guard just like Haskell '|'
    % ~s prints Name as STRING, ~n is the NEWLINE char.
    io:fwrite("Hello ~s~n", [Name]),
    % date() returns a tuple {year,month,day}
    {CurrentYear, _, _} = date(),
    BirthYear = CurrentYear - Age,
    % ~p is the pretty print!
    io:fwrite("You were born in ~p~n", [BirthYear]);
% '_' is a dont-care, will match anything
greet_adult(_, _) ->
    io:fwrite("Hey kid!").

% A note on statement endings:
% a statement can terminate with either ';', ',' or '.'

% We use ',' for separating multiple statements inside
% of a function. The last one will be either '.' or ';'.
% When we declare a function multiple times with pattern
% matching, we use ';' for all declarations except the last one,
% for which we use '.'

% A Person (which is a map) has:
% - name (string)
% - age (number)
% - likes (list of atoms)
greet_person(#{name := Name, age := Age, likes := Likes}) ->
    Greeting =
        if
            Age >= 18 ->
                case lists:member(soccer, Likes) of
                    true -> "Hello " ++ Name ++ ", I like soccer, too!";
                    false -> "Hello " ++ Name
                end;
            Age < 18 ->
                "Hey kid!"
        end,
    % rewrite it with case
    Greeting.

% We have the 'cons' as '|' and list comprehensions with '||'
get_how_long([]) -> 0;
get_how_long([_ | Xs]) -> 1 + get_how_long(Xs).

% MAPS
% Map = #{key => 0}.
% Updated = Map#{key := 1}.
% #{key := Value} = Updated.
% Value =:= 1.
% %=> true

% A word on comparisons:
% Erlang has lax equality and strict equality, just like JavaScript
% '==' is to compare two values, possibly of different type (float VS integer)
% '=:=' is to evaluate comparisons between two items of the same type, false otherwise.
% 1 == 1.0  % => True
% 1 =:= 1.0 % => False

% Apply works just like you expect, with a twist: the first
% parameter is the module. Remember that the list you pass
% is the list of ARGUMENTS.
