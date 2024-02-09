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

## Static calls (Optional)

When capabale, `Rc<impl StaticClosure<(P1, P2, ...), R>` can be used to avoid dynamic dispatching. For example, there may be immediate apply operation or the `rura` compiler may know that a function is feasible for static dispatch. In such cases, `rura` may use static opaque type instead:

```rust
    fn test_closure2(
        f: Rc<impl StaticClosure<(i32, i32), i32>>,
        x: i32,
        y: i32,
    ) -> Rc<impl StaticClosure<(), i32>> {
        f.static_apply(x).static_apply(y)
    }
```

## Erased Closure 

Closures that demand variable length argument or large-size context shall be compiled into `ErasedThunk` that no longer
keep full type information at Rust level. The type metadata will be maniputed by `rura` to generate correct code.

```rust
#[repr(C)]
pub union ArgPacket {
    scalar: usize,
    pointer: *const (),
    boxed_dyn: ManuallyDrop<Box<Rc<dyn Any>>>,
    boxed_scalar: ManuallyDrop<Box<i128>>,
}

pub struct ErasedThunk<R> {
    code: fn(alloc::vec::Vec<ArgPacket>) -> R,
    params: alloc::vec::Vec<ArgPacket>,
}
```
The parameters pack, in this case, are stored in a dynamic array that stores machine words. For parameter that demands larger size or alignment than machine word, an additional box layer will be used.