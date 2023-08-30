# Compiling Haskell

To run an Haskell program, you need to:

1. Open the terminal
2. Compile the program with `ghc -o <output name> <input name>`
3. Run the executable file with `./<output name>`

> You also need a `main` entrypoint

Of course this sounds super repetitive. In our case, we can use `ghci` instead:

1. Open the terminal
2. Run `ghci` to open the interactive REPL
3. Import your module with `:load <module>.hs`
4. Call the function you want to run!