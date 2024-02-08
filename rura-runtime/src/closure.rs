use core::mem::MaybeUninit;

use alloc::rc::Rc;

pub trait PartialParams {
    type Next;
    type Full;
    type Progress: PartialParams<Full = Self::Full>;
    fn apply(&mut self, next: Self::Next);
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
    type Next = ();
    type Progress = ();
    type Full = ();
    fn apply(&mut self, _: Self::Next) {}
}

impl<T> PartialParams for (Ready<T>,) {
    type Next = ();
    type Progress = (Ready<T>,);
    type Full = (Ready<T>,);
    fn apply(&mut self, _: Self::Next) {}
}

impl<T> PartialParams for (Hole<T>,) {
    type Next = T;
    type Progress = (Ready<T>,);
    type Full = (Ready<T>,);
    fn apply(&mut self, next: Self::Next) {
        self.0 .0.write(next);
    }
}

impl<A, B> PartialParams for (Ready<A>, Ready<B>) {
    type Next = ();
    type Progress = (Ready<A>, Ready<B>);
    type Full = (Ready<A>, Ready<B>);
    fn apply(&mut self, _: Self::Next) {}
}

impl<A, B> PartialParams for (Ready<A>, Hole<B>) {
    type Next = B;
    type Progress = (Ready<A>, Ready<B>);
    type Full = (Ready<A>, Ready<B>);
    fn apply(&mut self, next: Self::Next) {
        self.1 .0.write(next);
    }
}

impl<A, B> PartialParams for (Hole<A>, Hole<B>) {
    type Next = A;
    type Progress = (Ready<A>, Hole<B>);
    type Full = (Ready<A>, Ready<B>);
    fn apply(&mut self, next: Self::Next) {
        self.0 .0.write(next);
    }
}

struct Thunk<P: PartialParams, R> {
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

impl<P: PartialParams, R> Thunk<P, R> {
    pub fn eval(self) -> R
    where
        P: PartialParams<Full = P>,
    {
        (self.code)(self.params)
    }
    pub fn apply(&mut self, x: P::Next) {
        self.params.apply(x);
    }
}

#[repr(transparent)]
pub struct Closure<P: PartialParams, R>(Rc<Thunk<P, R>>);

impl<P: PartialParams + Clone, R> Clone for Closure<P, R> {
    fn clone(&self) -> Self {
        Closure(self.0.clone())
    }
}

impl<P: PartialParams + Clone, R> Closure<P, R> {
    pub fn apply(mut self, x: P::Next) -> Closure<P::Progress, R> {
        unsafe {
            Rc::make_mut(&mut self.0).apply(x);
            core::mem::transmute(self)
        }
    }
    pub fn eval(self) -> R
    where
        P: PartialParams<Full = P>,
    {
        let f = Rc::unwrap_or_clone(self.0);
        f.eval()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_closure() {
        let f = Closure(Rc::new(Thunk {
            code: |(Ready(a), Ready(b)): (Ready<i32>, Ready<i32>)| a + b,
            params: (Hole(MaybeUninit::uninit()), Hole(MaybeUninit::uninit())),
        }));
        assert_eq!(f.apply(1).apply(2).eval(), 3);
    }
}
