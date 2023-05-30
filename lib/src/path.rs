use crate::{Parameter, Return, ReturnResult};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;

/// Any type that implements the Function trait can be added as a function to a Path
pub trait Function {
    /// The dispatch method provides the functionality that is execuated when an associated function is called
    fn dispatch(&self, parameters: &[Parameter]) -> ReturnResult;
}

macro_rules! make_impl {
    ($params:literal, $($param:ident, $error:ident, $name:literal, $idx:literal),*) => {
        impl<R, $($param, $error),*> Function for &(dyn Fn($($param),*) -> R) where Return: From<R>, $($param: TryFrom<Parameter, Error = $error>, $error: Display),* {
            fn dispatch(&self, parameters: &[Parameter]) -> ReturnResult {
                let len = parameters.len();
                if len != $params {
                    return Err(format!("expected {} parameters, received {len}", $params));
                }
                Ok(self(
                    $(parameters[$idx].clone().try_into().map_err(|x: $error|format!("error with {} parameter: {}", $name, x))?),*
                ).into())
            }
        }

        impl<R, RE, $($param, $error),*> Function for &(dyn Fn($($param),*) -> Result<R, RE>) where Return: From<R>, String: From<RE>, $($param: TryFrom<Parameter, Error = $error>, $error: Display),* {
            fn dispatch(&self, parameters: &[Parameter]) -> ReturnResult {
                let len = parameters.len();
                if len != $params {
                    return Err(format!("expected {} parameters, received {len}", $params));
                }
                Ok(self(
                    $(parameters[$idx].clone().try_into().map_err(|x: $error|format!("error with {} parameter: {}", $name, x))?),*
                )?.into())
            }
        }
    }
}

make_impl!(0,);
make_impl!(1, P0, E0, "first", 0);
make_impl!(2, P0, E0, "first", 0, P1, E1, "second", 1);
make_impl!(3, P0, E0, "first", 0, P1, E1, "second", 1, P2, E2, "third", 2);
make_impl!(4, P0, E0, "first", 0, P1, E1, "second", 1, P2, E2, "third", 2, P3, E3, "fourth", 3);

impl<R> Function for &dyn Fn(&[Parameter]) -> R
where
    Return: From<R>,
{
    fn dispatch(&self, parameters: &[Parameter]) -> ReturnResult {
        Ok(self(parameters).into())
    }
}

impl<R, RE> Function for &dyn Fn(&[Parameter]) -> Result<R, RE>
where
    Return: From<R>,
    String: From<RE>,
{
    fn dispatch(&self, parameters: &[Parameter]) -> ReturnResult {
        Ok(self(parameters)?.into())
    }
}

enum PathType<'a> {
    SubPath(Box<Path<'a>>),
    Function(&'a dyn Function),
}

/// The Path struct defines a path within the proc macro environment.
pub struct Path<'a> {
    subpaths: HashMap<String, PathType<'a>>,
}

impl<'a> Path<'a> {
    /// Creates a new Path
    pub fn new() -> Self {
        let subpaths = HashMap::new();
        Self { subpaths }
    }

    /// Add a subpath to this path.
    ///
    /// # Panics
    /// This method will panic if the identifier is already in use under this path or the provided
    /// identifier is invalid.
    pub fn add_path(&mut self, ident: &str, path: Path<'a>) {
        match self.subpaths.entry(ident.to_string()) {
            Entry::Vacant(entry) => entry.insert(PathType::SubPath(Box::new(path))),
            Entry::Occupied(entry) => panic!(
                "proc macro environment is faulty: '{}' has been added as a subpath multiple times",
                entry.key()
            ),
        };
    }

    /// Add a function to this path.
    ///
    /// # Panics
    /// This method will panic if the identifier is already in use under this path or the provided
    /// identifier is invalid.
    pub fn add_function(&mut self, ident: &str, function: &'a dyn Function) {
        match self.subpaths.entry(ident.to_string()) {
            Entry::Vacant(entry) => entry.insert(PathType::Function(function)),
            Entry::Occupied(entry) => panic!(
                "proc macro environment is faulty: '{}' has been added as a subpath multiple times",
                entry.key()
            ),
        };
    }

    pub(super) fn get_path(&self, ident: &str) -> Option<&Path> {
        match self.subpaths.get(ident) {
            Some(PathType::SubPath(path)) => Some(path.as_ref()),
            _ => None,
        }
    }

    pub(super) fn get_function(&self, ident: &str) -> Option<&dyn Function> {
        match self.subpaths.get(ident) {
            Some(PathType::Function(function)) => Some(*function),
            _ => None,
        }
    }
}

impl<'a> Default for Path<'a> {
    fn default() -> Self {
        Self::new()
    }
}
