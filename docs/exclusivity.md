# Exclusivity (is all you need)

## A "side effect" of linearity

We know that linear type is a good thing that it captures side effects of functional programming. Due to linearity, you
can mutate the object in-place without breaking the "functional expectation" of values.

However, another interesting "side effect" of linearity is that, you can actually wrap foreign imperative objects as a
linear type and mutate it in its original way. For example, Lean4's [`Array`] is just implemented in the same way as
how C++'s `std::vector` is made:

```c++
extern "C" LEAN_EXPORT object * lean_array_push(obj_arg a, obj_arg v) {
    object * r;
    if (lean_is_exclusive(a)) {
        if (lean_array_capacity(a) > lean_array_size(a))
            r = a;
        else
            r = lean_copy_expand_array(a, true);
    } else {
        r = lean_copy_expand_array(a, lean_array_capacity(a) < 2*lean_array_size(a) + 1);
    }
    lean_assert(lean_array_capacity(r) > lean_array_size(r));
    size_t & sz  = lean_to_array(r)->m_size;
    object ** it = lean_array_cptr(r) + sz;
    *it = v;
    sz++;
    return r;
}
```

In general, if an imperative object is `Rc`-tracked, and it supports `Clone` that does not diverge the behavior of
different copies, then one can easily wrap a normal imperative functions to a functional programming language by calling
[`Rc::make_mut`] right before each call to the mutator function.

[`Array`]: https://lean-lang.org/lean4/doc/array.html

[`Rc::make_mut`]: https://doc.rust-lang.org/std/rc/struct.Rc.html#method.make_mut

## Explicit annotation of in-place updates

There are limitations of this `Rc`-tracked approach, however. The following discussion is taken from Lean.

Consider the following loop that add `1.0` to every element in a `FloatArray`:

```lean
partial def add1(x : FloatArray) : FloatArray :=
  let rec loop (@[exclusive] r : FloatArray) (i : Nat) : FloatArray :=
    if h : i < r.size then
      let idx : Fin r.size := ⟨ i, h ⟩
      loop (r.set idx (r.get idx + 1.0)) (i+1)
    else
      r
  loop x 0
```

The code effectively compiles to:

```c
LEAN_EXPORT lean_object *l_add1_loop(lean_object *x_1, lean_object *x_2) {
_start: {
  lean_object *x_3;
  uint8_t x_4;
  x_3 = lean_float_array_size(x_1);
  x_4 = lean_nat_dec_lt(x_2, x_3);
  lean_dec(x_3);
  if (x_4 == 0) {
    lean_dec(x_2);
    return x_1;
  } else {
    double x_5;
    double x_6;
    double x_7;
    lean_object *x_8;
    lean_object *x_9;
    lean_object *x_10;
    x_5 = lean_float_array_fget(x_1, x_2);
    x_6 = l_add1_loop___closed__1;
    x_7 = lean_float_add(x_5, x_6);
    x_8 = lean_float_array_fset(x_1, x_2, x_7);
    x_9 = lean_unsigned_to_nat(1u);
    x_10 = lean_nat_add(x_2, x_9);
    lean_dec(x_2);
    x_1 = x_8;
    x_2 = x_10;
    goto _start;
  }
}
}
```

Which, however, cannot be vectorized due to control dependencies from `lean_float_array_fset` and `lean_dec`.

The `lean_dec` thing is not hard to tackle, we could simply provide something like `lean_float_array_fset_usize`
or `lean_float_array_fget_usize` that uses scalars as induction variables. One can also wrap it as `FinUsize` in the
language to make it type-safe. In Rura, we will use normal `usize` anyway.

The exclusivity check in `lean_float_array_fset` is somehow more complicated to resolve. The compiler needs to have the
knowledge of "once being exclusive, always being exclusive", and compile the function in a "flow-sensitive" way that
lifts exclusivity checking outside the loop. Or, we just have a uniqueness type system (with affine/linear types)
alongside FBIP. However, this may separate the runtime into two parts: RC-tracked objects and unique-boxed objects.

[FP^2: Fully in-Place Functional Programming] is definitely a good attempt to reduce such complication. Basically, FP^2
colors functions and embed fully in-place updates into RC-based reuse analysis. However, it seems to me that it still
performs dynamic uniqueness checking within the functions colored as `fip`.

We can actually have a more adaptive way. Looking into the definition of `ensure_exclusivity` of `array` (`sarray` is
similar).

```
static inline lean_obj_res lean_ensure_exclusive_array(lean_obj_arg a) {
    if (lean_is_exclusive(a)) return a;
    return lean_copy_array(a);
}
```

We can actually see that this operation should be easily generalized to all Lean objects (Just provide "shallow copy"
for all objects). So, how about having a `lean_ensure_exclusive` in the IR (just as `inc/dec`)?

Then, we can mark the loop as:

```lean
partial def add1(x : FloatArray) : FloatArray :=
  let rec loop (@[exclusive] r : FloatArray) (i : Nat) : FloatArray :=
    if h : i < r.size then
      let idx : Fin r.size := ⟨ i, h ⟩
      loop (r.set idx (r.get idx + 1.0)) (i+1)
    else
      r
  loop x 0
```

Inside the loop function, all uniqueness checking can be removed. Instead, upon calling the function from a normal
one, we call `lean_ensure_exclusivity` on the objects being passed. What's more, calls among "exclusive" functions
should not require such checks.

There should still be (affine-)linearity check inside the exclusive functions, to ensure that the "exclusive" object:

- either dropped
- or used (passed to other function or returned exactly once)

Besides, the "exclusive" object can be used multiple times by `borrow` functions. Here `borrow` may be more restrictive
than what we already have in Lean in the sense that it should not modify the refcount.

However, such checks are only performed locally within the "exclusive" functions without polluting the whole type
system. Also, the runtime implementation is not complicated. Compared with linear types, this does seem more "adaptive"
as you can still make calls between "normal" functions and "exclusive" functions without propagating the "color" of
functions.

In Rura, we use a separate `Unique<T>` as the explicit annotation of exclusivity. See also [Ownership](ownership.md).

[FP^2: Fully in-Place Functional Programming]: https://www.microsoft.com/en-us/research/publication/fp2-fully-in-place-functional-programming/
