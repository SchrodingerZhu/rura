use core::mem::MaybeUninit;

use alloc::rc::Rc;

use crate::MemoryReuse;

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
