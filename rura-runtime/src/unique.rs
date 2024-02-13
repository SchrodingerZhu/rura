use core::{
    cell::UnsafeCell,
    ops::{Deref, DerefMut},
    ptr::NonNull,
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

    fn drop_for_reuse<U>(mut self) -> crate::ReuseToken<U>
    where
        T: Sized,
    {
        let self_layout: core::alloc::Layout = core::alloc::Layout::new::<T>();
        let target_layout: core::alloc::Layout = core::alloc::Layout::new::<U>();
        if self_layout != target_layout {
            return ReuseToken::Invalid;
        }
        let ptr = Rc::into_raw(unsafe { self.0.take().unwrap_unchecked() }) as *mut T;
        core::mem::forget(self);
        unsafe {
            core::ptr::drop_in_place(ptr);
            ReuseToken::Valid(NonNull::new_unchecked(ptr.cast()))
        }
    }

    fn from_token(value: Self::Target, token: crate::ReuseToken<Self::Target>) -> Self
    where
        T: Sized,
    {
        Unique(Some(Rc::from_token(UnsafeCell::new(value), unsafe {
            core::mem::transmute(token)
        })))
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
        unsafe {
            let rc = self.0.take().unwrap_unchecked();
            crate::assert_unchecked(rc.is_exclusive());
        }
    }
}
