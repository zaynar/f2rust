//! Context object for thread-safe implementation of `SAVE`.
//!
//! This stores a single instance of each type `T: SaveInit`, where each `T`
//! should represent the `SAVE` variables of a single program unit
//! (which may be shared by multiple entries).
//!
//! `ctx.get_vars::<T>()` will use `T::new()` on the first call, then return the same
//! `T` on subsequent calls.
//!
//! It actually returns an `Rc<RefCell<T>>`, for dynamic lifetime and borrow-checking.
//!
//! Since FORTRAN 77 does not allow recursive procedures, it is safe for a function
//! to use `borrow_mut()` and hold the reference until it returns.
//!
//! `Context` also provides a way to intercept and/or sandbox any IO.

use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};

use crate::{Error, Result, io::WriterBuilder};

pub trait SaveInit {
    fn new() -> Self;
}

pub struct Context<'a> {
    data: HashMap<TypeId, Rc<dyn Any>>,

    // This is Rc<RefCell> because we lend it out to WriterBuilder, which might
    // hold onto it across other calls into Context
    stdout: Rc<RefCell<dyn std::io::Write + 'a>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            stdout: Rc::new(RefCell::new(std::io::stdout())),
        }
    }

    pub fn get_vars<T: 'static + SaveInit>(&mut self) -> Rc<RefCell<T>> {
        let obj = self
            .data
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Rc::new(RefCell::new(T::new())));

        Rc::downcast::<RefCell<T>>(Rc::clone(obj)).unwrap()
    }

    pub fn set_stdout<W: std::io::Write + 'a>(&mut self, stdout: W) {
        self.stdout = Rc::new(RefCell::new(stdout));
    }

    /// STOP statement
    pub fn stop(&self) -> Result<()> {
        Err(Error::Terminated(0))
    }

    /// EXIT intrinsic
    pub fn exit(&self, status: &[i32]) -> Result<()> {
        Err(Error::Terminated(*status.first().unwrap_or(&0)))
    }

    pub fn writer<'w>(&mut self) -> WriterBuilder<'w>
    where
        'a: 'w,
    {
        WriterBuilder::new(Rc::clone(&self.stdout))
    }
}

impl Default for Context<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct SaveVars1 {
        x: i32,
    }

    struct SaveVars2 {
        x: i32,
    }

    impl SaveInit for SaveVars1 {
        fn new() -> Self {
            Self { x: 1 }
        }
    }

    impl SaveInit for SaveVars2 {
        fn new() -> Self {
            Self { x: 100 }
        }
    }

    #[test]
    fn test_saved() {
        let mut ctx = Context::new();

        {
            let s1 = ctx.get_vars::<SaveVars1>();
            let s1 = &mut *s1.borrow_mut();

            assert_eq!(s1.x, 1);
            s1.x += 1;
            assert_eq!(s1.x, 2);

            saved_sub(&mut ctx, 100);
            saved_sub(&mut ctx, 110);

            assert_eq!(s1.x, 2);
        }

        {
            let s1 = ctx.get_vars::<SaveVars1>();
            let s1 = &mut *s1.borrow_mut();
            assert_eq!(s1.x, 2);
        }
    }

    fn saved_sub(ctx: &mut Context, exp: i32) {
        let s2 = ctx.get_vars::<SaveVars2>();
        let s2 = &mut *s2.borrow_mut();
        assert_eq!(s2.x, exp);
        s2.x += 10;
    }
}
