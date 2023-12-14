%% A simple dependency. All it does is
%% exporting x() that returns a constant.
%% Try changing the output of x() from '1' to '2',
%% and recompile the module while the ex3 other part
%% is still running and see what happens!
-module(ex3b_lib).
-export([x/0]).

x() -> 3.
