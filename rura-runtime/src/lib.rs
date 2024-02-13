#![no_std]
#![cfg_attr(
    any(feature = "nightly", test),
    feature(hint_assert_unchecked, fn_traits, unboxed_closures)
)]
use alloc::rc::Rc;
use core::{mem::MaybeUninit, ops::Deref, ptr::NonNull};
mod closure;
mod unique;
extern crate alloc;

pub use closure::*;
pub use unique::Unique;

#[inline(always)]
unsafe fn assert_unchecked(x: bool) {
    #[cfg(feature = "nightly")]
    unsafe {
        core::hint::assert_unchecked(x);
    }
    #[cfg(not(feature = "nightly"))]
    unsafe {
        if !x {
            core::hint::unreachable_unchecked();
        }
    }
}

pub enum ReuseToken<T> {
    Invalid,
    Valid(NonNull<MaybeUninit<T>>),
}

impl<T> ReuseToken<T> {
    pub fn is_valid(&self) -> bool {
        matches!(self, ReuseToken::Valid(..))
    }
    pub fn layout(&self) -> core::alloc::Layout {
        core::alloc::Layout::new::<T>()
    }
}

impl<T> Drop for ReuseToken<T> {
    fn drop(&mut self) {
        if let ReuseToken::Valid(rc, ..) = self {
            unsafe {
                let rc = Rc::from_raw(rc.as_ptr());
                crate::assert_unchecked(rc.is_exclusive());
            }
        }
    }
}

pub trait MemoryReuse: Deref {
    fn is_exclusive(&self) -> bool;
    #[must_use]
    fn drop_for_reuse(self) -> ReuseToken<Self::Target>
    where
        Self::Target: Sized;
    fn from_token<U: Sized>(value: Self::Target, token: ReuseToken<U>) -> Self
    where
        Self::Target: Sized;
}

impl<T: ?Sized> MemoryReuse for Rc<T> {
    #[inline(always)]
    fn is_exclusive(&self) -> bool {
        Rc::strong_count(self) == 1 && Rc::weak_count(self) == 0
    }
    fn drop_for_reuse(self) -> ReuseToken<T>
    where
        T: Sized,
    {
        if self.is_exclusive() {
            let ptr = Rc::into_raw(self) as *mut T;
            unsafe {
                core::ptr::drop_in_place(ptr);
                ReuseToken::Valid(NonNull::new_unchecked(ptr.cast()))
            }
        } else {
            ReuseToken::Invalid
        }
    }
    fn from_token<U: Sized>(value: Self::Target, token: ReuseToken<U>) -> Self
    where
        T: Sized,
    {
        let token_layout = token.layout();
        let value_layout = core::alloc::Layout::new::<T>();
        if token_layout != value_layout {
            return Rc::new(value);
        }
        match token {
            ReuseToken::Invalid => Rc::new(value),
            ReuseToken::Valid(ptr, ..) => unsafe {
                core::mem::forget(token);
                let mut ptr: NonNull<MaybeUninit<T>> = ptr.cast();
                ptr.as_mut().write(value);
                Rc::from_raw(ptr.as_ptr().cast())
            },
        }
    }
}

pub trait Exclusivity: MemoryReuse {
    fn make_mut(&mut self) -> &mut Self::Target;
    fn uniquefy(self) -> Unique<Self::Target>;
}

impl<T: ?Sized + Clone> Exclusivity for Rc<T> {
    fn make_mut(&mut self) -> &mut Self::Target {
        Rc::make_mut(self)
    }

    fn uniquefy(self) -> Unique<Self::Target> {
        self.into()
    }
}

#[cfg(test)]
mod test {
    extern crate std;
    use super::*;
    /*
    rura! {
     enum List {
        Nil,
        Cons(i32, List),
     }

     fn add(xs : List, val: i32) -> List {
        match xs {
            Nil => Nil,
            Cons(x, xs) => Cons(x + val, add_one(xs)),
        }
     }
    }
    */

    #[derive(Clone, Debug)]
    enum List<T> {
        Nil,
        Cons(T, Rc<Self>),
    }

    fn add<T: core::ops::Add<U> + Clone, U: Clone>(xs: Rc<List<T>>, val: U) -> Rc<List<T::Output>> {
        match *xs {
            List::Nil => {
                let token = xs.drop_for_reuse();
                Rc::from_token(List::Nil, token)
            }
            List::Cons(ref y, ref ys) => {
                let new_xs = add(ys.clone(), val.clone());
                let y = y.clone();
                let token = xs.drop_for_reuse();
                Rc::from_token(List::Cons(y + val, new_xs), token)
            }
        }
    }

    #[test]
    fn test_add() {
        let xs = Rc::new(List::Cons(1, Rc::new(List::Cons(2, Rc::new(List::Nil)))));
        let ys = add(xs, 1);
        std::println!("{:?}", ys);
        use std::string::String;
        let xs = Rc::new(List::Cons(
            String::from("123"),
            Rc::new(List::Cons(String::from("234"), Rc::new(List::Nil))),
        ));
        let ys = add(xs, "456");
        std::println!("{:?}", ys);
    }
}
