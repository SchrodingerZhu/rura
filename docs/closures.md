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

When capabable, `Rc<impl StaticClosure<(P1, P2, ...), R>` can be used to avoid dynamic dispatching. For example, there may be immediate apply operation or the `rura` compiler may know that a function is feasible for static dispatch. In such cases, `rura` may use static opaque type instead:

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
pub union ScalarPack {
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    f32: f32,
    f64: f64,
    usize: usize,
}

#[derive(Clone)]
pub enum BoxedPack {
    U128(u128),
    I128(i128),
    Object(Rc<dyn Any>),
}

pub struct ErasedThunk<R> {
    code: fn(Vec<ScalarPack>, Vec<BoxedPack>) -> R,
    scalar: Vec<ScalarPack>,
    boxed: Vec<BoxedPack>,
}
```
The parameters pack, in this case, are stored in a dynamic array that stores machine words. For parameter that demands larger size or alignment than machine word, an additional box layer will be used.