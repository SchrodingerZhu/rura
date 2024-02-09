use core::{any::Any, mem::MaybeUninit};

use alloc::{rc::Rc, vec::Vec};

use crate::{Exclusivity, MemoryReuse};

pub trait Params {
    type Head;
    type Tail: Params;
}

rura_internal_macros::impl_parameters!();

pub trait PartialParams: Clone {
    type Pending: Params;
    type Full;
    type Progress: PartialParams<Full = Self::Full, Pending = <Self::Pending as Params>::Tail>;
    fn apply(&mut self, next: <Self::Pending as Params>::Head);
    unsafe fn transmute_full(self) -> Self::Full;
}

#[repr(transparent)]
pub struct Hole<T>(MaybeUninit<T>);
impl<T> Clone for Hole<T> {
    fn clone(&self) -> Self {
        Hole(MaybeUninit::uninit())
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct Ready<T>(T);

rura_internal_macros::generate_all_partial_param_impls!();
rura_internal_macros::generate_from_impls!();

pub struct Thunk<P: PartialParams, R> {
    code: fn(P::Full) -> R,
    params: P,
}

impl<P: PartialParams + Clone, R> Clone for Thunk<P, R> {
    fn clone(&self) -> Self {
        Thunk {
            code: self.code,
            params: self.params.clone(),
        }
    }
}
pub trait BoxedClosure<P: Params, R> {
    fn apply(self: Rc<Self>, param: P::Head) -> Rc<dyn BoxedClosure<P::Tail, R>>;
    fn eval(self: Rc<Self>) -> R
    where
        P: Params<Head = ()>;
}

pub trait StaticClosure<P: Params, R>: BoxedClosure<P, R> {
    fn static_apply(self: Rc<Self>, param: P::Head) -> Rc<impl StaticClosure<P::Tail, R>>;
}

impl<P: PartialParams + Clone + 'static, R: 'static> BoxedClosure<P::Pending, R> for Thunk<P, R> {
    fn apply(
        mut self: Rc<Self>,
        param: <P::Pending as Params>::Head,
    ) -> Rc<dyn BoxedClosure<<P::Pending as Params>::Tail, R>> {
        let thunk = Rc::make_mut(&mut self);
        thunk.params.apply(param);
        let raw = Rc::into_raw(self);
        unsafe {
            let rc = Rc::from_raw(raw as *const Thunk<P::Progress, R>);
            crate::assert_unchecked(rc.is_exclusive());
            rc
        }
    }

    fn eval(self: Rc<Self>) -> R
    where
        P::Pending: Params<Head = ()>,
    {
        let thunk = Rc::unwrap_or_clone(self);
        unsafe { (thunk.code)(thunk.params.transmute_full()) }
    }
}

impl<P: PartialParams + Clone + 'static, R: 'static> StaticClosure<P::Pending, R> for Thunk<P, R> {
    fn static_apply(
        mut self: Rc<Self>,
        param: <P::Pending as Params>::Head,
    ) -> Rc<impl StaticClosure<<<P as PartialParams>::Pending as Params>::Tail, R>> {
        let thunk = Rc::make_mut(&mut self);
        thunk.params.apply(param);
        let raw = Rc::into_raw(self);
        unsafe {
            let rc = Rc::from_raw(raw as *const Thunk<P::Progress, R>);
            crate::assert_unchecked(rc.is_exclusive());
            rc
        }
    }
}

#[repr(transparent)]
pub struct Closure<P, R>(Rc<dyn BoxedClosure<P, R>>);

impl<P, R> Clone for Closure<P, R> {
    fn clone(&self) -> Self {
        Closure(self.0.clone())
    }
}

impl<P: Params, R> Closure<P, R> {
    pub fn apply(self, param: P::Head) -> Closure<P::Tail, R> {
        Closure(self.0.apply(param))
    }

    pub fn eval(self) -> R
    where
        P: Params<Head = ()>,
    {
        self.0.eval()
    }
}

#[cfg(feature = "nightly")]
impl<P: Params, R> FnOnce<(P::Head,)> for Closure<P, R> {
    type Output = Closure<P::Tail, R>;
    extern "rust-call" fn call_once(self, x: (P::Head,)) -> Self::Output {
        self.apply(x.0)
    }
}

#[cfg(feature = "nightly")]
impl<P: Params<Head = ()>, R> FnOnce<()> for Closure<P, R> {
    type Output = R;
    extern "rust-call" fn call_once(self, _: ()) -> Self::Output {
        self.eval()
    }
}

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

impl Clone for ScalarPack {
    fn clone(&self) -> Self {
        unsafe { core::ptr::read(self) }
    }
}

pub struct ErasedThunk<R> {
    code: fn(Vec<ScalarPack>, Vec<BoxedPack>) -> R,
    scalar: Vec<ScalarPack>,
    boxed: Vec<BoxedPack>,
}

impl<R> Clone for ErasedThunk<R> {
    fn clone(&self) -> Self {
        ErasedThunk {
            code: self.code,
            scalar: self.scalar.clone(),
            boxed: self.boxed.clone(),
        }
    }
}

impl<R> ErasedThunk<R> {
    pub fn new(fn_ptr: fn(Vec<ScalarPack>, Vec<BoxedPack>) -> R) -> Rc<Self> {
        Rc::new(ErasedThunk {
            code: fn_ptr,
            scalar: Vec::new(),
            boxed: Vec::new(),
        })
    }

    pub fn apply_scalar(mut self: Rc<Self>, arg: ScalarPack) -> Rc<Self> {
        self.make_mut().scalar.push(arg);
        self
    }

    pub fn apply_i128(mut self: Rc<Self>, arg: i128) -> Rc<Self> {
        self.make_mut().boxed.push(BoxedPack::I128(arg));
        self
    }

    pub fn apply_u128(mut self: Rc<Self>, arg: u128) -> Rc<Self> {
        self.make_mut().boxed.push(BoxedPack::U128(arg));
        self
    }

    pub fn apply_object<T: 'static>(mut self: Rc<Self>, arg: Rc<T>) -> Rc<Self> {
        self.make_mut().boxed.push(BoxedPack::Object(arg));
        self
    }

    pub fn apply_cloure<P: Params, T>(mut self: Rc<Self>, arg: Closure<P, T>) -> Rc<Self> {
        self.make_mut()
            .boxed
            // this is extremely dirty but miri does seem to be happy with it
            .push(BoxedPack::Object(unsafe { core::mem::transmute(arg) }));
        self
    }

    pub fn eval(self: Rc<Self>) -> R {
        let thunk = Rc::unwrap_or_clone(self);
        (thunk.code)(thunk.scalar, thunk.boxed)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_erased_closure_push_closure() {
        let f = ErasedThunk::new(|scalar, objects| {
            let a = unsafe { scalar[0].i32 };
            let b = unsafe { scalar[1].i32 };
            let mut objects = objects.into_iter();
            let Some(BoxedPack::Object(obj)) = objects.next() else {
                unreachable!()
            };
            let obj: Closure<(i32, i32), i32> = unsafe { core::mem::transmute(obj) };
            obj.apply(a).apply(b).eval()
        });
        f.clone().apply_scalar(ScalarPack { i32: 1 });
        let g = f
            .apply_scalar(ScalarPack { i32: 1 })
            .apply_scalar(ScalarPack { i32: 2 })
            .apply_cloure(Closure(Rc::new(Thunk {
                code: |(x, y): (i32, i32)| x + y,
                params: (Hole(MaybeUninit::uninit()), Hole(MaybeUninit::uninit())),
            })));
        let h = g.clone();
        assert_eq!(h.eval(), 3);
    }

    fn test_closure(f: Closure<(i32, i32), i32>, x: i32, y: i32) -> Closure<(), i32> {
        f.apply(x).apply(y)
    }

    fn test_closure2(
        f: Rc<impl StaticClosure<(i32, i32), i32>>,
        x: i32,
        y: i32,
    ) -> Rc<impl StaticClosure<(), i32>> {
        f.static_apply(x).static_apply(y)
    }

    #[test]
    fn test() {
        let f = Closure(Rc::new(Thunk {
            code: |(x, y)| x + y,
            params: (Hole(MaybeUninit::uninit()), Hole(MaybeUninit::uninit())),
        }));
        let g = test_closure(f, 1, 2);
        assert_eq!(g.eval(), 3);
    }

    #[test]
    fn test_static() {
        let f = Rc::new(Thunk {
            code: |(x, y)| x + y,
            params: (Hole(MaybeUninit::uninit()), Hole(MaybeUninit::uninit())),
        });
        assert_eq!(f.clone().static_apply(13).static_apply(23).eval(), 36);
        assert_eq!(test_closure2(f, 13, 23).eval(), 36);
    }

    #[cfg(feature = "nightly")]
    #[test]
    fn test_nightly() {
        use alloc::format;
        use alloc::string::String;

        let f = Closure(Rc::new(Thunk {
            code: |(x, y): (i32, Rc<String>)| format!("x: {x}, y : {y}"),
            params: (Hole(MaybeUninit::uninit()), Hole(MaybeUninit::uninit())),
        }));
        assert_eq!(f(1)(String::from("1234").into())(), "x: 1, y : 1234")
    }

    #[test]
    fn test_long_tuple() {
        let f: fn((i32, i32, i32, i32, i32, i32)) -> i32 =
            |(a, b, c, d, e, f): (i32, i32, i32, i32, i32, i32)| a + b + c + d + e + f;
        let f = Closure::from(f);
        let g = f.apply(1).apply(2).apply(3);
        let h = g.clone();
        assert_eq!(g.apply(2).apply(2).apply(3).eval(), 13);
        assert_eq!(h.apply(4).apply(5).apply(6).eval(), 21);
    }
}
