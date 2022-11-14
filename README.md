# Programming Languages 2 (2020-2021)


---
---
## Table of Contents


- [Ex1: Haskell Energy Tree](#haskell-energy-tree)
- [Ex2: Haskell Fibonacii Trees and Tree Outlines](#haskell-fibonacii-trees-and-tree-outlines)
- [Ex3: Erlang Rally](#erlang-rally)
- [Ex4: Erlang Property Based Testing](#erlang-property-based-testing)
- [Ex5: Reverse Hash](#reverse-hash)
- [Ex6: Type Systems - letrec](#type-systems---letrec)
- [Ex7: Type Inference](#type-inference)
- [Ex8: Axiomatic Semantics](#axiomatic-semantics)
- [Ex9: OpCode VM](#opcode-vm)
- [Ex10: Scripting Languages](#scripting-languages)
- [Ex11: Static Analysis](#static-analysis)

---
---

## [<ins>Haskell Energy Tree</ins>](./ex1/)

Recursive solution implemented in haskell, for the problem of finding the balance vertex of a weighted tree. The balance vertex is the vertex that minimizes the maximum weight of all its subtrees.

---

## [<ins>Haskell Fibonacii Trees and Tree Outlines</ins>](./ex2/)

Fibonacii Trees and Tree Outlines with Haskell. Different implementation of functions for creating and reversing trees and returning tree outlines. Both naive and optimized algorithms are implemented, that take advantage of Haskell's graph reduction.

---

## [<ins>Erlang Rally</ins>](./ex3/)

Simple algorithmic problem to get accustomed with writing in Erlang. The problem can be solved fully recursively with greedy step, or using dynamic programming.

The solution is also accompanied by unit, and property-based tests (the latter are executed using [PropEr](https://github.com/proper-testing/proper)).

---

## [<ins>Erlang Property Based Testing</ins>](./ex4/)

Given a simple programming language's syntax and specifications, used for evaluating vector expressions, along with 50 evaluator implementation, find which of them have bugs using property-based testing ([PropEr](https://github.com/proper-testing/proper)).

---

## [<ins>Reverse Hash</ins>](./ex5/)

Find the reverse image of the given hash, for a specified hash function, using brute force approach. The implementation should scale with multiple schedulers.

- [x] Parallel Erlang
- [ ] Parallel Haskell
- [ ] Concurrent Haskell

---

## [<ins>Type Systems - letrec</ins>](./ex6/)

Theoritical exercise on type systems. Trying to define a "let rec" structure that allows the use of mutual recursion.

---

## [<ins>Type Inference</ins>](./ex7/)

Implementing the type inference for simple lambda calculus (using Haskell).

---

## [<ins>Axiomatic Semantics</ins>](./ex8/)

Proving the correctness of a computer program (written in C) through axiomatic semantics approach.

The assertions are defined in the program file and can be validated using:
- frama-c -> v22.0 (Titanium)
- Alt-Ergo -> v2.4.0

---

## [<ins>OpCode VM</ins>](./ex9/)

Implementation of a Virtual Machine that executes a stack-based, opcode programming language.

The VM is accompanied by a Garbage Collector that gets executed in order to free up the heap memory during runtime, using the Mark-and-Sweep approach.

---

## [<ins>Scripting Languages</ins>](./ex10/)

A webpage written in PHP, that hosts a simple game of Morse Code translation.

A client is also implemented in Python3 that plays the game.

![Webpage](ex10/Screenshot%20from%202022-11-14%2020-03-41.png)

---

## [<ins>Static Analysis</ins>](./ex11/)

Three exercises from the online book [Static Program Analysis](https://cs.au.dk/~amoeller/spa/spa.pdf) of Anders Møller and Michael I. Schwartzbach.