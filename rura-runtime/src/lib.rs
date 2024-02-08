#![no_std]
#![cfg_attr(feature = "nightly", feature(hint_assert_unchecked))]
use alloc::rc::Rc;
use core::{mem::MaybeUninit, ops::Deref, ptr::NonNull};
mod closure;
mod unique;
extern crate alloc;

pub use closure::*;
pub use unique::Unique;

#[inline(always)]
fn assert_unchecked(x: bool) {
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
        if let ReuseToken::Valid(ptr, ..) = self {
            unsafe {
                let rc = Rc::from_raw(ptr.as_ref());
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
    unsafe fn from_token<U>(value: Self::Target, token: ReuseToken<U>) -> Self
    where
        Self::Target: Sized;
}

impl<T: ?Sized> MemoryReuse for Rc<T> {
    #[inline(always)]
    fn is_exclusive(&self) -> bool {
        Rc::strong_count(self) == 1 && Rc::weak_count(self) == 0
    }
    fn drop_for_reuse(self) -> ReuseToken<Self::Target>
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
    unsafe fn from_token<U>(value: Self::Target, token: ReuseToken<U>) -> Self
    where
        T: Sized,
    {
        debug_assert_eq!(core::mem::size_of::<T>(), token.layout().size());
        debug_assert_eq!(core::mem::align_of::<T>(), token.layout().align());
        match token {
            ReuseToken::Invalid => Rc::new(value),
            ReuseToken::Valid(ptr, ..) => unsafe {
                core::mem::forget(token);
                let mut ptr = ptr.cast::<MaybeUninit<T>>();
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
    enum List {
        Nil,
        Cons(i32, Rc<List>),
    }

    fn add(xs: Rc<List>, val: i32) -> Rc<List> {
        match *xs {
            List::Nil => xs,
            List::Cons(y, ref ys) => {
                let new_xs = add(ys.clone(), val);
                let token = xs.drop_for_reuse();
                unsafe { Rc::from_token(List::Cons(y + val, new_xs), token) }
            }
        }
    }

    #[test]
    fn test() {
        let xs = Rc::new(List::Cons(1, Rc::new(List::Cons(2, Rc::new(List::Nil)))));
        let ys = add(xs, 1);
        std::println!("{:?}", ys);
    }
}
