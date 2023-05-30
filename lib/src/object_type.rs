use crate::{Object, Parameter, Return, ReturnResult};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

/// Any type that implements the Method trait can be added as an immutable method to an ObjectType
pub trait Method<T> {
    /// The dispatch method provides the functionality that is execuated when an associated method on an object is called
    fn dispatch(&self, object: &T, parameters: &[Parameter]) -> ReturnResult;
}

/// Any type that implements the MethodMut trait can be added as a mutable method to an ObjectType
pub trait MethodMut<T> {
    /// The dispatch method provides the functionality that is execuated when an associated method on an object is called
    fn dispatch(&self, object: &mut T, parameters: &[Parameter]) -> ReturnResult;
}

macro_rules! make_impl {
    ($params:literal, $($param:ident, $error:ident, $name:literal, $idx:literal),*) => {
        impl<T: 'static, R, $($param, $error),*> Method<T> for &(dyn Fn(&T, $($param),*) -> R) where Return: From<R>, $($param: TryFrom<Parameter, Error = $error>, $error: Display),* {
            fn dispatch(&self, object: &T, parameters: &[Parameter]) -> ReturnResult {
                let len = parameters.len();
                if len != $params {
                    return Err(format!("expected {} parameters, received {len}", $params));
                }
                Ok(self(object,
                    $(parameters[$idx].clone().try_into().map_err(|x: $error|format!("error with {} parameter: {}", $name, x))?),*
                ).into())
            }
        }

        impl<T: 'static, R, RE, $($param, $error),*> Method<T> for &(dyn Fn(&T, $($param),*) -> Result<R, RE>) where Return: From<R>, String: From<RE>, $($param: TryFrom<Parameter, Error = $error>, $error: Display),* {
            fn dispatch(&self, object: &T, parameters: &[Parameter]) -> ReturnResult {
                let len = parameters.len();
                if len != $params {
                    return Err(format!("expected {} parameters, received {len}", $params));
                }
                Ok(self(object,
                    $(parameters[$idx].clone().try_into().map_err(|x: $error|format!("error with {} parameter: {}", $name, x))?),*
                )?.into())
            }
        }

        impl<T: 'static, R, $($param, $error),*> MethodMut<T> for &(dyn Fn(&mut T, $($param),*) -> R) where Return: From<R>, $($param: TryFrom<Parameter, Error = $error>, $error: Display),* {
            fn dispatch(&self, object: &mut T, parameters: &[Parameter]) -> ReturnResult {
                let len = parameters.len();
                if len != $params {
                    return Err(format!("expected {} parameters, received {len}", $params));
                }
                Ok(self(object,
                    $(parameters[$idx].clone().try_into().map_err(|x: $error|format!("error with {} parameter: {}", $name, x))?),*
                ).into())
            }
        }

        impl<T: 'static, R, RE, $($param, $error),*> MethodMut<T> for &(dyn Fn(&mut T, $($param),*) -> Result<R, RE>) where Return: From<R>, String: From<RE>, $($param: TryFrom<Parameter, Error = $error>, $error: Display),* {
            fn dispatch(&self, object: &mut T, parameters: &[Parameter]) -> ReturnResult {
                let len = parameters.len();
                if len != $params {
                    return Err(format!("expected {} parameters, received {len}", $params));
                }
                Ok(self(object,
                    $(parameters[$idx].clone().try_into().map_err(|x: $error|format!("error with {} parameter: {}", $name, x))?),*
                )?.into())
            }
        }
    };
}

make_impl!(0,);
make_impl!(1, P0, E0, "first", 0);
make_impl!(2, P0, E0, "first", 0, P1, E1, "second", 1);
make_impl!(3, P0, E0, "first", 0, P1, E1, "second", 1, P2, E2, "third", 2);
make_impl!(4, P0, E0, "first", 0, P1, E1, "second", 1, P2, E2, "third", 2, P3, E3, "fourth", 3);

impl<T: 'static, R> Method<T> for &dyn Fn(&T, &[Parameter]) -> R
where
    Return: From<R>,
{
    fn dispatch(&self, object: &T, parameters: &[Parameter]) -> ReturnResult {
        Ok(self(object, parameters).into())
    }
}

impl<T: 'static, R, RE> Method<T> for &dyn Fn(&T, &[Parameter]) -> Result<R, RE>
where
    Return: From<R>,
    String: From<RE>,
{
    fn dispatch(&self, object: &T, parameters: &[Parameter]) -> ReturnResult {
        Ok(self(object, parameters)?.into())
    }
}

impl<T: 'static, R> MethodMut<T> for &dyn Fn(&T, &[Parameter]) -> R
where
    Return: From<R>,
{
    fn dispatch(&self, object: &mut T, parameters: &[Parameter]) -> ReturnResult {
        Ok(self(object, parameters).into())
    }
}

impl<T: 'static, R, RE> MethodMut<T> for &dyn Fn(&T, &[Parameter]) -> Result<R, RE>
where
    Return: From<R>,
    String: From<RE>,
{
    fn dispatch(&self, object: &mut T, parameters: &[Parameter]) -> ReturnResult {
        Ok(self(object, parameters)?.into())
    }
}

enum MethodType<T: 'static> {
    Ref(&'static dyn Method<T>),
    Mut(&'static dyn MethodMut<T>),
}

/// The ObjectType struct is used to define the methods of an object type. Once all methods are
/// called, the `ObjectType::seal` method should be called. This consumes the ObjectType and returns
/// a SealedObjectType that can be used to instantiate objects.
pub struct ObjectType<T: 'static> {
    methods: HashMap<String, MethodType<T>>,
}

impl<T: 'static> ObjectType<T> {
    /// Creates a new ObjectType
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
        }
    }

    /// Adds a method to the ObjectType
    pub fn add_method(&mut self, name: &str, method: &'static dyn Method<T>) {
        self.methods
            .insert(name.to_string(), MethodType::Ref(method));
    }

    /// Adds a method to the ObjectType that allows mutation of the object
    pub fn add_method_mut(&mut self, name: &str, method: &'static dyn MethodMut<T>) {
        self.methods
            .insert(name.to_string(), MethodType::Mut(method));
    }

    /// Consumes the ObjectType and returns a SealedObjectType. The SealedObjectType is immutable
    /// but is required to instantiate objects.
    pub fn seal(self) -> SealedObjectType<T> {
        SealedObjectType::new(self.methods)
    }
}

/// The SealedObjectType is obtained by calling the `ObjectType::seal` method. The SealedObjectType
/// is immutable, but allows the instantiation of objects.
pub struct SealedObjectType<T: 'static> {
    mutable: bool,
    methods: Rc<HashMap<String, MethodType<T>>>,
}

impl<T: 'static> SealedObjectType<T> {
    fn new(methods: HashMap<String, MethodType<T>>) -> Self {
        Self {
            mutable: methods.values().any(|x| matches!(x, MethodType::Mut(_))),
            methods: Rc::new(methods),
        }
    }

    /// Creates a new instance of the SealedObjectType. Takes ownership of the passed in object.
    /// All methods called against the returned Object will receive a reference to the passed in
    /// object.
    pub fn new_instance(&self, object: T) -> Object {
        let methods = self.methods.clone();

        if self.mutable {
            let object = RefCell::new(object);
            Object::new(move |ident, method, params| {
                let Some(method) = methods.get(method) else {
                    return Err(match ident {
                        Some(ident) => format!("'{method}' is not a method on object '{ident}'"),
                        None => format!("'{method}' is not a method on object"),
                    });
                };

                let ident = match ident {
                    Some(ident) => format!("'{ident}'"),
                    None => "object".to_string(),
                };

                match *method {
                    MethodType::Ref(method) => Method::dispatch(
                        method,
                        &*object
                            .try_borrow()
                            .map_err(|err| format!("could not borrow {ident}: {err}"))?,
                        params,
                    ),
                    MethodType::Mut(method) => MethodMut::dispatch(
                        method,
                        &mut *object
                            .try_borrow_mut()
                            .map_err(|err| format!("could not mutably borrow {ident}: {err}"))?,
                        params,
                    ),
                }
            })
        } else {
            Object::new(move |ident, method, params| {
                let Some(MethodType::Ref(method)) = methods.get(method) else {
                    return Err(match ident {
                        Some(ident) => format!("'{method}' is not a method on object '{ident}'"),
                        None => format!("'{method}' is not a method on object"),
                    });
                };

                Method::dispatch(*method, &object, params)
            })
        }
    }
}

impl<T: 'static> Default for ObjectType<T> {
    fn default() -> Self {
        Self::new()
    }
}
