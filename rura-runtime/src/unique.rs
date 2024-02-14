use core::{
    cell::UnsafeCell,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
};

use alloc::rc::Rc;

use crate::{Exclusivity, MemoryReuse, ReuseToken};

/// We cannot use `UniqueRc` as it uses a single weak reference to represent the exclusivity. We just provide
/// a wrapper that makes it normially different from `Rc` and `Arc`.
/// `UnsafeCell<T>`` is of transparent layout of T. Therefore, we can directly wrap the inner cell to get mutability.
#[repr(transparent)]
pub struct Unique<T: ?Sized>(Option<Rc<UnsafeCell<T>>>);

impl<T: ?Sized> Unique<T> {
    pub fn new(value: T) -> Self
    where
        T: Sized,
    {
        Unique(Some(Rc::new(UnsafeCell::new(value))))
    }
    // Notice that [`Rc::get_mut`] with [`Option::unwrap_unchecked`] does not generate clean code.
    pub fn get_mut(&mut self) -> &mut T {
        unsafe { &mut *self.0.as_deref().unwrap_unchecked().get() }
    }
}

impl<T: ?Sized + Clone> From<Rc<T>> for Unique<T> {
    fn from(mut rc: Rc<T>) -> Self {
        rc.make_mut();
        unsafe { core::mem::transmute(rc) }
    }
}

impl<T: ?Sized> From<Unique<T>> for Rc<T> {
    fn from(unique: Unique<T>) -> Self {
        unsafe {
            let rc: Rc<T> = core::mem::transmute(unique);
            crate::assert_unchecked(rc.is_exclusive());
            rc
        }
    }
}

impl<T: ?Sized> Deref for Unique<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { &*self.0.as_deref().unwrap_unchecked().get() }
    }
}

impl<T: ?Sized> DerefMut for Unique<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.get_mut()
    }
}

impl<T: ?Sized> MemoryReuse for Unique<T> {
    #[inline(always)]
    fn is_exclusive(&self) -> bool {
        true
    }

    fn drop_for_reuse(mut self) -> crate::ReuseToken<T>
    where
        T: Sized,
    {
        let ptr: *mut MaybeUninit<T> = Rc::into_raw(unsafe { self.0.take().unwrap_unchecked() })
            .cast_mut()
            .cast();
        unsafe {
            (*ptr).assume_init_drop();
            ReuseToken::valid(Rc::from_raw(ptr))
        }
    }

    fn from_token<U: Sized>(value: Self::Target, token: crate::ReuseToken<U>) -> Self
    where
        T: Sized,
    {
        Unique(Some(Rc::from_token(UnsafeCell::new(value), token)))
    }

    fn unwrap_for_reuse(mut self) -> (ReuseToken<Self::Target>, Self::Target)
    where
        Self::Target: Sized + Clone,
    {
        let ptr: *mut MaybeUninit<T> = Rc::into_raw(unsafe { self.0.take().unwrap_unchecked() })
            .cast_mut()
            .cast();
        unsafe {
            (
                ReuseToken::valid(Rc::from_raw(ptr)),
                (*ptr).assume_init_read(),
            )
        }
    }
}

impl<T: ?Sized> Exclusivity for Unique<T> {
    fn make_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }

    fn uniquefy(self) -> Unique<Self::Target> {
        self
    }
}

impl<T: ?Sized> Drop for Unique<T> {
    fn drop(&mut self) {
        if let Some(rc) = self.0.take() {
            unsafe {
                crate::assert_unchecked(rc.is_exclusive());
            }
        }
    }
}
