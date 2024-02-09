# rura
RUst way for Reuse Analysis.

Reuse analysis is invented to reduce the heavy allocation cost in RC-based functional programming runtimes. Since we have reference counting, we actually

## Reuse Analysis and Linear Type Literature

1. [Perceus](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf)
2. [Linear type can change the world](https://cs.ioc.ee/ewscs/2010/mycroft/linear-2up.pdf)
3. [Frame-limited reuse](https://www.microsoft.com/en-us/research/publication/reference-counting-with-frame-limited-reuse-extended-version/)
4. [Fully in-place Function Programming](https://www.microsoft.com/en-us/research/uploads/prod/2023/05/fbip.pdf)

Additionally, the idea of reduce duplication and improve sharing has also been discussed in some other situations:

5. [HVM](https://github.com/HigherOrderCO/HVM/blob/master/guide/HOW.md)
6. [Optimal Sharing](https://www.researchgate.net/publication/235778993_The_optimal_implementation_of_functional_programming_languages)

## Why This Project?

In Koka and Lean, Reuse Analysis is supported by a lightweight runtime with RC and type-erased objects. This project attempts to create a Functional Programming DSL that directly lowers into Rust itself. In this way, we can explore the possibility to embed reuse analysis into higher-level languages.

## Design & Goals

0. Reuse Rust:
    We want to check the possibility to reuse the type system in rust directly:
    - It took Koka a very long time before introducing a working type-class/overloading system. In `rura`, we can explore if we can use its trait-system directly.
    - Rust has move semantics, reference system with borrow checking, lifetime analysis. Heavy checks are usually unfriendly for the purpose of being a codegen target. However, we actually want to utilize these checks to guarantee the correctness/efficiency of the code. 

1. Frame-limited Reuse Analysis. 
   We want to perform frame-limited reuse analysis for `rura`. This will deliver much higher performance than plain RC based memory management.

2. Seamless Interpolation.
   A side effect of RC-based reuse analysis is that you can easily wrap non-function data into a functional one. This actually enable `rura` to directly interact with Rust.

## Language Design

1. Surface Syntax Specification (TODO)
2. [Types](docs/types.md)
3. [Ownership Model](docs/ownership.md)
4. [Linearity/Exclusivity](docs/exclusivity.md)
5. [Closures](docs/closures.md)

