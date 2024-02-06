use alloc::rc::Rc;
use core::{mem::MaybeUninit, ptr::NonNull};
extern crate alloc;

enum ReuseToken<T> {
    Invalid,
    Valid(NonNull<MaybeUninit<T>>),
}

impl<T> ReuseToken<T> {
    fn is_valid(&self) -> bool {
        matches!(self, ReuseToken::Valid(..))
    }
    fn layout(&self) -> core::alloc::Layout {
        core::alloc::Layout::new::<T>()
    }
}

impl<T> Drop for ReuseToken<T> {
    fn drop(&mut self) {
        if let ReuseToken::Valid(ptr, ..) = self {
            unsafe {
                Rc::from_raw(ptr.as_ref());
            }
        }
    }
}

trait MemoryReuse {
    type Object: Sized;
    fn is_exclusive(&self) -> bool;
    #[must_use]
    fn drop_for_reuse(self) -> ReuseToken<Self::Object>;
    unsafe fn from_token<U>(value: Self::Object, token: ReuseToken<U>) -> Self;
}

impl<T: Sized> MemoryReuse for Rc<T> {
    type Object = T;
    fn is_exclusive(&self) -> bool {
        Rc::strong_count(&self) == 1 && Rc::weak_count(&self) == 0
    }
    fn drop_for_reuse(self) -> ReuseToken<Self::Object> {
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
    unsafe fn from_token<U>(value: Self::Object, token: ReuseToken<U>) -> Self {
        debug_assert_eq!(core::mem::size_of::<T>(), token.layout().size());
        debug_assert_eq!(core::mem::align_of::<T>(), token.layout().align());
        match token {
            ReuseToken::Invalid => Rc::new(value),
            ReuseToken::Valid(ptr, ..) => unsafe {
                core::mem::forget(token);
                let mut ptr = ptr.cast::<MaybeUninit<T>>();
                ptr.as_mut().write(value);
                Rc::from_raw(ptr.as_ref().assume_init_ref())
            },
        }
    }
}

trait FieldReuse: MemoryReuse {
    fn make_mut(&mut self) -> &mut Self::Object;
}

impl<T: Clone> FieldReuse for Rc<T> {
    fn make_mut(&mut self) -> &mut Self::Object {
        Rc::make_mut(self)
    }
}

#[cfg(test)]
mod test {
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
        println!("{:?}", ys);
    }
}
