# Ownership Model

## Inspirations

- [Pony's reference-capabilities](https://tutorial.ponylang.io/reference-capabilities/reference-capabilities.html)
- [Verona](https://microsoft.github.io/verona/)
- [FP^2](https://www.microsoft.com/en-us/research/uploads/prod/2023/05/fbip.pdf)

## Types of references

- `Rc<T>` (default): Target is reference-counting managed. This is the only pointer that can be materialized (i.e.
  stored as a field in Rura's composite types)
- `Unique<T>`: Target is reference-counting managed but is known to be unique. This improves the performance in some
  cases. See [Linearity/Exclusivity](exclusivity.md)
- `&T`: A reference to a managed object without claiming ownership. Notice that reference in Rura is translated
  into `&T` rather than `&Rc<T>`, such that the holder of this reference will not mess around the `Rc` internal counts

```plaintext
            ┏━━━━━━━━━┓      deref
            ┃   Rc    ┃─────────────────┐ 
            ┗━━━━━━━━━┛                 │
             │      ↑                   ↓
make_mut()   │      │                ┏━━━━━━━━━┓ 
(clone)      │      │ direct         ┃   &T    ┃ 
             │      │                ┗━━━━━━━━━┛
             ↓      │                   ↑
            ┏━━━━━━━━━┓                 │
            ┃ Unique  ┃─────────────────┘      
            ┗━━━━━━━━━┛     deref

```

## Details

As stated above, `Rc` is the default ownership. For example, in following Rura structure (inductive type):

```rust
enum List<T> {
    Nil,
    Cons(T, List<T>),
}
```

Is actually represented as this in Rust:

```rust
#[derive(Clone)]
enum List<T> {
    Nil,
    Cons(T, Rc<T>),
}
```

Also, in function params, variables, or whatever places without other special annotations, when referring to Rura
inductive types, they are passed/stored as `Rc` by default. For example, `List<List<i32>>` is actually
`Rc<List<Rc<List<i32>>>>`.

When declaring functions, however, if the parameter type is annotated with `!`, it means that the function requires the
parameter to be exclusive, hence a `Unique<T>` is passed. In generated code, `Rc<T>` will be converted to `Unique<T>` by
calling [`Into::into`], which may trigger an underlying [`Clone::clone`] operation.

```rust
fn foo<T>(x: !List<T>)
```

Again, in function declarations, if the parameter type is annotated with `&`, it means that the function takes the
direct reference.

```rust
fn foo<T>(x: &List<T>)
```

As said above, `&List<T>` really means `&List<T>` rather than `&Rc<List<T>>`. A reference can be obtained from
both `Rc<T>` and `Unique<T>` using [`Deref`].

This is needed for seamless interaction with Rust. For efficiency, one may want to pass Rura managed objects to Rust
function directly via references, and if `&Rc` is passed, target Rust code may be able to increase the reference counts
and break the assumptions inside Rura.

[`Into::into`]: https://doc.rust-lang.org/std/convert/trait.Into.html#tymethod.into

[`Clone::clone`]: https://doc.rust-lang.org/std/clone/trait.Clone.html#tymethod.clone

[`Deref`]: https://doc.rust-lang.org/std/ops/trait.Deref.html
