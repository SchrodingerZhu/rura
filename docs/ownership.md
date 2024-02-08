# Ownership Model

## Inspiring Designs

- [reference-capabilities](https://tutorial.ponylang.io/reference-capabilities/reference-capabilities.html)
- [Verona](https://microsoft.github.io/verona/)
- [FP^2](https://www.microsoft.com/en-us/research/uploads/prod/2023/05/fbip.pdf)

## Types of References

- `Rc<T>`: target is reference-counting managed (default). This is the only pointer that can be materialized (i.e. stored as a field in `rura`'s composite types).
- `Unique<T>`:  target is reference-counting managed but is known to be unique. This is used to improve performance in some cases. See [Linearity/Exclusivity](exclusivity.md).
- `&T`: A reference to a managed object without claiming ownership. (Notice that reference in `rura` is translated into `&T` rather than `&Rc<T>`, such that the holder of this reference cannot clone the `Rc` pointer) 

```
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

As stated above, `Rc` is the default ownership. For example, in following `rura` structure (inductive type): 
```rust
enum List<T> {
  Nil,
  Cons(T, List<T>),
}
```
is actually represented as
```rust
#[derive(Clone)]
enum List<T> {
  Nil,
  Cons(T, Rc<T>),
}
```
in Rust.

Also, in function params, variables, or whatever place without other special annotation, when refering to `rura` indutive types, they are passed/stored as `Rc` by default. For example, `List<List<i32>>` is actually `Rc<List<Rc<List<i32>>>>`.

When declaring functions, however, if the parameter type is annotated with `!`, it means that the function requires the parameter to be exclusive, hence a `Unique<T>` is passed. In generated code, `Rc<T>` will be converted to `Unique<T>` by calling [`Into::into`], which may trigger a underlying [`Clone::clone`] operation.
```rust
fn foo<T>(x : !List<T>)
```

Again, in function declarations,  if the parameter type is annotated with `&`, it means that the function takes the reference. 
```rust
fn foo<T>(x : &List<T>)
```
As said above, `&List<T>` really means `&List<T>` rather than `&Rc<List<T>>`. A reference can be obtained from both `Rc<T>` and `Unique<T>` using [`Deref`].

This is needed seamless interaction with Rust. For efficiency, one may want to pass `rura` managed objects to Rust function directly via reference. If `&Rc` is passed, target Rust code may be able to increase the reference counts and break assumptions inside `rura`.