#![no_std]
#![cfg_attr(
    any(feature = "nightly", test),
    feature(hint_assert_unchecked, fn_traits, unboxed_closures)
)]
use alloc::rc::Rc;
use core::{mem::MaybeUninit, ops::Deref};
mod closure;
mod hole;
mod unique;
extern crate alloc;

pub use closure::*;
pub use hole::Hole;
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

#[repr(transparent)]
pub struct ReuseToken<T>(Option<Rc<MaybeUninit<T>>>);

impl<T> ReuseToken<T> {
    pub fn is_valid(&self) -> bool {
        self.0.is_some()
    }
    pub fn layout(&self) -> core::alloc::Layout {
        core::alloc::Layout::new::<T>()
    }
    pub fn valid(rc: Rc<MaybeUninit<T>>) -> Self {
        ReuseToken(Some(rc))
    }
    pub fn invalid() -> Self {
        ReuseToken(None)
    }
}

impl<T> Drop for ReuseToken<T> {
    fn drop(&mut self) {
        if let Some(rc, ..) = self.0.take() {
            unsafe {
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
    fn unwrap_for_reuse(self) -> (ReuseToken<Self::Target>, Self::Target)
    where
        Self::Target: Sized + Clone;
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
            let ptr: *mut MaybeUninit<T> = Rc::into_raw(self).cast_mut().cast();
            unsafe {
                (*ptr).assume_init_drop();
                ReuseToken::valid(Rc::from_raw(ptr))
            }
        } else {
            ReuseToken::invalid()
        }
    }
    fn from_token<U: Sized>(value: Self::Target, mut token: ReuseToken<U>) -> Self
    where
        T: Sized,
    {
        let token_layout = token.layout();
        let value_layout = core::alloc::Layout::new::<T>();
        if token_layout != value_layout {
            return Rc::new(value);
        }
        match token.0.take() {
            None => Rc::new(value),
            Some(rc) => unsafe {
                let ptr: *mut MaybeUninit<T> = Rc::into_raw(rc).cast_mut().cast();
                (*ptr).write(value);
                Rc::from_raw(ptr.cast())
            },
        }
    }

    fn unwrap_for_reuse(self) -> (ReuseToken<Self::Target>, Self::Target)
    where
        Self::Target: Sized + Clone,
    {
        if self.is_exclusive() {
            let ptr: *mut MaybeUninit<T> = Rc::into_raw(self).cast_mut().cast();
            unsafe {
                let value = (*ptr).assume_init_read();
                (ReuseToken::valid(Rc::from_raw(ptr)), value)
            }
        } else {
            (ReuseToken::invalid(), (*self).clone())
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
    use alloc::string::String;

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
        #[allow(dead_code)]
        Cons(T, Rc<Self>),
        ToDrop(Rc<Self>),
    }

    fn update_string(s: Rc<String>) -> Rc<String> {
        let (tk, mut s) = s.unwrap_for_reuse();
        s.push_str("13");
        Rc::from_token(s, tk)
    }

    fn update_head(mut xs: Rc<List<Rc<String>>>) -> Rc<List<Rc<String>>> {
        match *xs {
            List::Nil => xs,
            List::Cons(..) => {
                if let List::Cons(ref mut y, ..) = *xs.make_mut() {
                    let (hole, s) = Hole::new(y);
                    let new_y = update_string(s);
                    hole.fill(new_y);
                    xs
                } else {
                    unsafe {
                        core::hint::unreachable_unchecked();
                    }
                }
            }
            List::ToDrop(..) => {
                if let (tk, List::ToDrop(tail)) = xs.unwrap_for_reuse() {
                    Rc::from_token(List::Cons(Rc::new("123".into()), tail), tk)
                } else {
                    unsafe {
                        core::hint::unreachable_unchecked();
                    }
                }
            }
        }
    }

    #[test]
    fn test_update_head() {
        let xs = Rc::new(List::Cons(
            Rc::new("".into()),
            Rc::new(List::Cons(Rc::new("123".into()), Rc::new(List::Nil))),
        ));
        let xs = Rc::new(List::ToDrop(xs));
        let ys = update_head(xs);
        let zs = update_head(ys);
        std::println!("{:?}", zs);
    }
}
