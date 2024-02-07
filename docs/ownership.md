# Ownership Model

## Inspiring Designs

- [reference-capabilities](https://tutorial.ponylang.io/reference-capabilities/reference-capabilities.html)
- [Verona](https://microsoft.github.io/verona/)
- [FP^2](https://www.microsoft.com/en-us/research/uploads/prod/2023/05/fbip.pdf)

## Types of References

- `Rc<T>`: target is reference-counting managed. This is the only pointer that can be materialized (i.e. stored as a field in `rura`'s composite types).
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