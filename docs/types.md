# Types

`rura` has both plain scalar types and memory-managed objects. The overall idea is to keep a similar design as Rust.

`rura` types are always defined with `Clone` trait.

## Scalar Types

- `bool`
- `i8/u8`
- `i16/u16`
- `i32/u32`
- `i64/u64`
- `i128/u128`
- `f32`
- `f64`
- `char`

## Unit Type

We use the same notion `()` for unit type as Rust.

## Bottom Type

We use the same notion `!` for bottom type (never type) as Rust. This represents the function never returns (i.e. it panics or exits). 

## Tuples

A Tuple is of the form `(A, B, C, D, ...)` where `A, B, C, D, ...` are other admissible types. Tuples are stored and passed without boxing.

## Inductive Types

```rust
enum Foo<T> {
    Bar,
    Baz(u64, Foo<T>),
    Qux(usize, T)
}
```

### Array/String/...

Current design is to directly use foreign wrappers. Details are to be decided.