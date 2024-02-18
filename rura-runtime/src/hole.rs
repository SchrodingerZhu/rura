pub struct Hole<'a, T: Unpin> {
    hole: Option<&'a mut T>,
}

impl<'a, T: Unpin> Hole<'a, T> {
    pub fn new(target: &'a mut T) -> (Self, T) {
        let val = unsafe { core::ptr::read(target) };
        (Self { hole: Some(target) }, val)
    }
    pub fn fill(mut self, val: T) {
        if let Some(hole) = self.hole.take() {
            unsafe {
                core::ptr::write(hole, val);
            }
        } else {
            #[cfg(debug_assertions)]
            panic!("hole filled twice");
            #[cfg(not(debug_assertions))]
            unsafe {
                core::hint::unreachable_unchecked();
            }
        }
    }
}

impl<'a, T: Unpin> Drop for Hole<'a, T> {
    fn drop(&mut self) {
        if self.hole.is_some() {
            panic!("unfilled hole detected");
        }
    }
}

#[cfg(test)]
mod tests {
    use core::any::Any;

    use crate::{Exclusivity, Hole};
    use alloc::rc::Rc;

    #[derive(Clone)]
    struct TestStruct {
        _unused: u8,
        a: Rc<usize>,
        b: Rc<dyn Any>,
    }

    #[test]
    fn test_multi_hole() {
        let test = Rc::new(TestStruct {
            _unused: 0,
            a: Rc::new(0),
            b: Rc::new(1),
        });
        let mut unique = test.uniquefy();
        let mut_ref = unique.make_mut();
        let (hole1, _rc1) = Hole::new(&mut mut_ref.a);
        let (hole2, _rc2) = Hole::new(&mut mut_ref.b);
        hole1.fill(Rc::new(2));
        hole2.fill(Rc::new(3));
    }
}
