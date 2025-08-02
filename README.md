# Haskell and Idris

Here are Haskell and Idris2 implementations of a `Garage` that holds vehicles,
only some of which require refueling. The Haskell code enforces this at
runtime, the Idris2 code enforces it in the types.

## Haskell

    $ cd haskell
    $ nix-shell
    $ cabal repl
    > testGarage

## Idris2

    $ cd idris
    $ nix-shell
    $ idris2 Garage.idr
    > :exec testGarage
