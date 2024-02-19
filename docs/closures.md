# Closures

Closures are, after all, `Rc`-managed objects. They are implemented in a manner of shared external objects, as each call fills
a pending argument and "mutate" the state of a closure. Hence, `Clone` operations may be needed if the closure if shared. Otherwise, it is updated linearly.

Generally, a closure object consists of a function pointer and its context. The context refers to both the objects captured during
closure creation and the objects that is incrementally passed into the closure.

## Fixed-length closures

Fixed length closure which context's length is at most 16 (to be decided), will be compiled into a `Thunk` struture, where `P` is
basically the tuple describing the parameter pack state of the closure.

```rust
pub struct Thunk<P: PartialParams, R> {
    code: fn(P::Full) -> R,
    params: P,
}
```

Such thunk will be packaged into `Rc<dyn BoxedClosure<(P1, P2, ...), R>` (a fat pointer) where `Pi` is the pending parameter types and `R` is the return type.

Notice that all `P1, ..., P2` should be clonable as provided by `rura`'s type system.

## Context 

Context will either be captured in a local object or using Rust's native closure capture.