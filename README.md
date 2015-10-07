# Introduction 

## What is Timeless RPG?

Timeless RPG is a RPG game framework based on [`timeless`][1], SDL2, and Haskell.

## What is its goal?

* The driving force for finishing `timeless` the library, and make `timeless` a usable general (A)FRP framework.
* To make games
* To make games easily
* To make games easily for new game programmers
* To make games easily for new programmers
* To make games easily for non programmers
* To make games easily for anyone
* To make games easily for monkeys typing on a typewriter

## Are you going to abandon this again?

Likely, but not as likely as before. The reason I abandoned my previous attempts using imperative languages and message passing structures was that although each component is tested to work by itself, I could not put them together. Additionally, testing the components are really, really, hard. But it is different here. Look at [2][2] [3][3] [4][4] and try to get a feeling how things written in `netwire` (`timeless` is a rewrite of `netwire`) can be tested and organized. If my intuition is correct, testing and combining using Arrows and pure functions will be much easier than mixing spaghetti code in my previous imperative engines. Hopefully I will get the chance to finish at least one simple, small, tiny RPG game like the style of RPG Maker games.

# Building

This package uses Stack. Make sure you have the [`timeless`][1] library downloaded too. Modify `stack.yaml` to include the correct relative path to `timeless` library. Hopefully `stack build` will just work.

[1]: https://github.com/carldong/timeless
[2]:http://stackoverflow.com/questions/30905930/what-can-be-a-minimal-example-of-game-written-in-haskell
[3]:http://stackoverflow.com/questions/30992299/console-interactivity-in-netwire
[4]:http://stackoverflow.com/questions/32745934/kleisli-arrow-in-netwire-5

