use core::{any::Any, mem::MaybeUninit};

use alloc::{rc::Rc, vec::Vec};

use crate::{Exclusivity, MemoryReuse};

pub trait Params {
    type Head;
    type Tail: Params;
}

impl Params for () {
    type Head = ();
    type Tail = ();
}

impl<A> Params for (A,) {
    type Head = A;
    type Tail = ();
}

impl<A, B> Params for (A, B) {
    type Head = A;
    type Tail = (B,);
}

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

impl PartialParams for () {
    type Progress = ();
    type Full = ();
    type Pending = ();
    fn apply(&mut self, _: ()) {}
    unsafe fn transmute_full(self) -> Self::Full {}
}

impl<T: Clone> PartialParams for (Ready<T>,) {
    type Pending = ();
    type Progress = (Ready<T>,);
    type Full = (T,);
    fn apply(&mut self, _: <Self::Pending as Params>::Head) {}
    unsafe fn transmute_full(self) -> Self::Full {
        (self.0 .0,)
    }
}

impl<T: Clone> PartialParams for (Hole<T>,) {
    type Pending = (T,);
    type Progress = (Ready<T>,);
    type Full = (T,);
    fn apply(&mut self, next: <Self::Pending as Params>::Head) {
        self.0 .0.write(next);
    }
    unsafe fn transmute_full(self) -> Self::Full {
        (self.0 .0.assume_init(),)
    }
}

impl<A: Clone, B: Clone> PartialParams for (Ready<A>, Ready<B>) {
    type Pending = ();
    type Progress = (Ready<A>, Ready<B>);
    type Full = (A, B);
    fn apply(&mut self, _: <Self::Pending as Params>::Head) {}
    unsafe fn transmute_full(self) -> Self::Full {
        (self.0 .0, self.1 .0)
    }
}

impl<A: Clone, B: Clone> PartialParams for (Ready<A>, Hole<B>) {
    type Pending = (B,);
    type Progress = (Ready<A>, Ready<B>);
    type Full = (A, B);
    fn apply(&mut self, next: <Self::Pending as Params>::Head) {
        self.1 .0.write(next);
    }
    unsafe fn transmute_full(self) -> Self::Full {
        (self.0 .0, self.1 .0.assume_init())
    }
}

impl<A: Clone, B: Clone> PartialParams for (Hole<A>, Hole<B>) {
    type Pending = (A, B);
    type Progress = (Ready<A>, Hole<B>);
    type Full = (A, B);
    fn apply(&mut self, next: <Self::Pending as Params>::Head) {
        self.0 .0.write(next);
    }
    unsafe fn transmute_full(self) -> Self::Full {
        (self.0 .0.assume_init(), self.1 .0.assume_init())
    }
}

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

    pub fn eval(self: Rc<Self>) -> R {
        let thunk = Rc::unwrap_or_clone(self);
        (thunk.code)(thunk.scalar, thunk.boxed)
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
}
