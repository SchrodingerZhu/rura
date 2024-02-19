# Rura

> RUst way for Reuse Analysis.

Rura is a purely functional Rust dialect, optimized through reuse analysis and more.

## Features

1. **Seamless interpolation** with Rust via code generation, while reusing other useful constructs like the trait system
2. Rust-native **reference counting** for memory management
3. Frame-limited **reuse analysis**, eliminating many of the allocations and other slow paths
4. **Uniqueness types (new!)** could boost the performance by getting rid of unnecessary refcount
5. **More scalar types** are available without any boxing

## Motivation

In [Koka] and [Lean], reuse analysis is supported by a lightweight runtime with reference counting and type-erased
objects. This project attempts to create a functional programming language that directly lowers into Rust. In this way,
we can explore the possibility to embed reuse analysis into higher-level languages.

[Koka]: http://koka-lang.org/

[Lean]: https://lean-lang.org/

## References

We implement the reuse analyzer based on:

1. [Perceus](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf)
2. [Frame-limited reuse](https://www.microsoft.com/en-us/research/publication/reference-counting-with-frame-limited-reuse-extended-version/)
3. [Fully in-place Function Programming](https://www.microsoft.com/en-us/research/uploads/prod/2023/05/fbip.pdf)

Uniqueness type and its linearity checking from:

1. [Linear type can change the world](https://cs.ioc.ee/ewscs/2010/mycroft/linear-2up.pdf)

Additionally, the idea of reduce duplication and improve sharing has also been discussed in some other situations:

1. [HVM](https://github.com/HigherOrderCO/HVM/blob/master/guide/HOW.md)
2. [Optimal Sharing](https://www.researchgate.net/publication/235778993_The_optimal_implementation_of_functional_programming_languages)

## Tours

1. Surface Syntax Specification (TODO)
2. [Types](docs/types.md)
3. [Ownership Model](docs/ownership.md)
4. [Linearity/Exclusivity](docs/exclusivity.md)
5. [Closures](docs/closures.md)
6. [Lowering](docs/lowering.md)
