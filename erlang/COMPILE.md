# Compiling Erlang

To compile an Erlang program, you need to:

1. Open the terminal
2. Run `erl`
3. Compile `c(<module name>).` with (or without) the `.erl` extension. You can use `pwd().` to check the current directory or use `cd(<Dir>).` to change it.
4. Run the methods you export from the module with `<module name>:<method name>(<params>).`

You can also use `erlc foo.erl` to compile the module _foo_ to `foo.beam`.

You can then run _foo_ using `erl -noshell -s foo start -s init stop`, where `start` is the method you want to call. `-s init stop` is needed to terminate the Erlang session. If you are a fan of Makefiles, this is the way to go.