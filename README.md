# Haskell and Idris2

Here are [Haskell](haskell/Garage.hs) and [Idris2](idris2/Garage.idr)
implementations of a `Garage` that holds vehicles, only some of which require
refueling. The Haskell code enforces this at runtime, the Idris2 code enforces
it in the types.

The example is based on Section 4.2.1 of
[Type-Driven Development with Idris](https://www.manning.com/books/type-driven-development-with-idris).
I've extended it to add a `Garage` that has a certain amount of fuel and a
number of vehicles, and a function to refuel the vehicles with the available
fuel.

    ```idris
    refuelAll : Garage -> Garage
    ```

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
