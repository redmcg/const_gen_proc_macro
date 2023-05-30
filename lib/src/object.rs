use crate::{Dispatch, Parameter, ReturnResult};
use std::rc::Rc;

/// The Object struct is used to represent an instantiation of an ObjectType. The Object struct is
/// created via the `SealedObjectType::new_instance` method.
#[derive(Clone)]
pub struct Object {
    dispatch: Rc<Dispatch>,
}

impl Object {
    pub(super) fn new(
        dispatch: impl Fn(Option<&str>, &str, &[Parameter]) -> ReturnResult + 'static,
    ) -> Self {
        Self {
            dispatch: Rc::new(dispatch),
        }
    }

    /// The call method allows calling a method of this Object struct. The method must be defined
    /// in the associated ObjectType, otherwise an error will be returned. The types and length of the params
    /// array must also be supported by the called function, other an error is returned.
    pub fn call(&self, method: &str, params: &[Parameter]) -> ReturnResult {
        (self.dispatch)(None, method, params)
    }

    pub(super) fn call_ident(
        &self,
        ident: &str,
        method: &str,
        params: &[Parameter],
    ) -> ReturnResult {
        (self.dispatch)(Some(ident), method, params)
    }
}
