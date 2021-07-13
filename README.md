# RayTracer
[![Build Status](https://travis-ci.com/twanbolwerkhan/haskell-raytracer-challenge.svg?branch=main)](https://travis-ci.com/twanbolwerkhan/haskell-raytracer-challenge)

This project is a challenge based on the book The raytracer challenge of Jamis Buck. Its entirely written in Haskell too make it even more challenging. It combines functional programming with lineair algebra learned as part of my pre-master.

## Prerequisite
https://www.haskell.org/downloads/
* ghci 8.10 >=
* cabal 2.0 >=

## How to run:

> cabal new-repl

 Chapter 1

> ghci> :l app/Chapter1

* enter velocity of projectile (for example use 11.5)

> ghci> 11.5

> ghci> :main

Chapter 4

> ghci> :l app/Chapter4

> ghci> :main


Chapter 5

> ghci> :l app/Chapter5

> ghci> :main


Chapter 6

* note this is gonna take some time.

> ghci> :l app/Chapter6

> ghci> :main

## How to test:

> cabal test

## Run tests with coverage
>  cabal configure --enable-coverage

## Configure with benchmarks

> cabal configure --enable-benchmarks && cabal build && cabal bench

## Run benchmarks

> cabal new-bench

> cabal bench --benchmark-option --output=bench.html

> rm Main.tix && cabal bench --benchmark-option --output=bench.html

## Static code analysis

Use stan for static code analysis

https://github.com/kowainik/stan

> stan report