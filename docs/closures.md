# Closures

Closures are, after all, `Rc`-managed objects. They are implemented in a manner of shared external objects, as each call
fills a pending argument and "mutates" the state of a closure. Hence, `Clone` operations may be needed if the closure is
shared. Otherwise, it is updated linearly.

Generally, a closure object consists of a function pointer and its context. The context refers to both the objects
captured during closure creation and the objects that are incrementally passed into the closure.

## Fixed-length closures

Fixed length closure, from which the context length is at most 16, will be compiled into a `Thunk` structure, where `P`
is basically the tuple describing the parameter pack state of the closure.

```rust
pub struct Thunk<P: PartialParams, R> {
    code: fn(P::Full) -> R,
    params: P,
}
```

Such thunk will be packaged into `Rc<dyn BoxedClosure<(P1, P2, ...), R>` (a fat pointer) where `P`<sub>i</sub> is the
pending parameter type and `R` is the return type.

Notice that all `P`<sub>i</sub> should be cloneable as provided by Rura's type system.

## Context

Context will either be captured in a local object or using Rust's native closure capturing.
