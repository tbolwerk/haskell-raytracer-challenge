# RayTracer

## Prerequisite
* haskell2010
* prefer haskell-platform
* ghcup
* ghci 8.10 >=
* cabal 2.0 >=

## How to run:

in root folder 
run: 
> cabal new-repl
> :l app/Scene
> :main

## How to test:
run:
> cabal test

## To add new benchmarks

> cabal configure --enable-benchmarks && cabal build && cabal bench

## TO run these benchmarks

> cabal new-bench
> cabal bench --benchmark-option --output=bench.html
> rm Main.tix && cabal bench --benchmark-option --output=bench.html