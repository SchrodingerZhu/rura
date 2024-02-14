use alloc::rc::Rc;

pub struct Hole<'a, T: ?Sized> {
    hole: Option<&'a mut Rc<T>>,
}

impl<'a, T: ?Sized> Hole<'a, T> {
    pub fn new(rc: &'a mut Rc<T>) -> (Self, Rc<T>) {
        let val = unsafe { core::ptr::read(rc) };
        (Self { hole: Some(rc) }, val)
    }
    pub fn fill(mut self, val: Rc<T>) {
        if let Some(hole) = self.hole.take() {
            unsafe {
                core::ptr::write(hole, val);
            }
        }
    }
}

impl<'a, T: ?Sized> Drop for Hole<'a, T> {
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
