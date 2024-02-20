# Lowering

This document describes the lower-level IR of Rura and how it is lowered into Rust.

## Basic Arithmetic

**TODO**: Support wrapping/saturating arithmetic.

## Eliminators

## Function Calls

## Object Management

## Closures

## Examples

First, an inductive type will be translated from something like:

```rust
enum List<T> {
    Nil,
    Cons(T, List<T>),
    Extra(List<T>, String),
}
```

Into:

```rust
enum List<T> {
    Nil,
    Cons(T, Rc<List<T>>),
    Extra(Rc<List<T>>, Rc<String>),
}
```

And `List<String>` is actually `Rc<List<Rc<String>>`.

Consider the following functions:

```rust
fn baz(List<String>) -> String;

fn bar(String) -> String;

fn foo(xs: List<String>) -> List<String> {
    match xs {
        List::Nil => List::Nil,
        List::Cons(a, b) => List::Cons(bar(a), b),
        List::Extra(x, _) => List::Cons(baz(), x)
    }
}
```

The initial IR should look like:

```rust
fn foo(% 0: List<String>) -> List<String> {
    match % 0
    {
        List::Nil => {
        %1 = new { ty: ::example::List, ty_args: ::std::String, ctor: Nil }
        return; % 1
    },
        List::Cons(0: %2, 1: %3) => {
        %4 = call::example::bar, % 2
            % 5 = new { ty: ::example::List, ty_args: ::std::String, ctor: Cons }, 0: % 4, 1: % 3
        return; % 5
    },
        List::Extra(0: %6, 1: %7) => {
        %8 = call::example::baz, % 6
            % 9 = new { ty: ::example::List, ty_args: ::std::String, ctor: Cons }, 0: % 7, 1: % 6
        return; % 9
    }
    }
}
```

### Pass 0: Drop/Clone Insertion

TODO
