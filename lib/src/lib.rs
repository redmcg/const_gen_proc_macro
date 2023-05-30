#![cfg_attr(feature = "proc_macro_expand", feature(proc_macro_expand))]

//! The goal of this library is to assist in the creation of proc macros, where the purpose of
//! the proc macro is to create constants. In addition, it sets out to achieve the following:
//! - the contents of the proc macro will use a subset of Rust syntax (for the purpose of readability);
//! - the proc macro will allow compile time functionality that might not be available to the target
//! platform (for example: the standard library or floating point arithmatic); and
//! - users of the proc macro will be given helpful complie time errors
//!
//! One use case for proc macros is to provide additional compile time functionality for the creation of
//! constants.
//!
//! A simple (although redundant) example of this would be to concatenate two literal strings
//! together (in the same vein as the concat! macro_rule).
//!
//! This library provides an API that makes the creation of such a proc macro trivial. For example,
//! the following code would be a complete proc macro providing said functionality:
//!
//! ```ignore
//! #[proc_macro]
//! pub fn string_concat(tokens: TokenStream) -> TokenStream {
//!     let mut string_path = Path::new();
//!     string_path.add_function(
//!         "concat",
//!         &(&|first: String, second: String| -> String {
//!             first + &second
//!         } as &dyn Fn(String, String) -> String),
//!    );
//!
//!    let mut env = ProcMacroEnv::new();
//!    env.add_path("string", string_path);
//!    env.process(tokens)
//! }
//! ```
//!
//! This code creates a proc macro "environment" with a single Path named "string". Under this Path
//! is a single function named "concat" that accepts two Strings and returns a String (which is the
//! concatenated result).
//!
//! Here is an example of code that makes use of this proc macro:
//!
//! ```ignore
//! string_concat! {
//!     let first = "First";
//!     let second = "Second";
//!     const STRING: &str = string::concat(first, second);
//! }
//!
//! assert_eq!(STRING, "FirstSecond");
//! ```
//!
//! Note that the two variables 'first' and 'second' are dropped from scope at the end of the
//! macro; only const values are preserved.
//!
//! It is also possible to create and use objects. For example, a "String" object with a "concat"
//! method can be created with the following code:
//!
//! ```ignore
//! #[proc_macro]
//! pub fn string_concat(tokens: TokenStream) -> TokenStream {
//!     let mut string_type = ObjectType::new();
//!     string_type.add_method(
//!         "concat",
//!         &(&|first: &String, second: String| -> String { first.to_owned() + &second }
//!             as &dyn Fn(&String, String) -> String),
//!     );
//!     // sealing the ObjectType means it is no longer mutable and can now instantiate objects
//!     let string_type = string_type.seal();
//!
//!     let mut string_path = Path::new();
//!     let string_new =
//!         &|first: String| -> Object { string_type.new_instance(first) } as &dyn Fn(String) -> Object;
//!     string_path.add_function("new", &string_new);
//!
//!     let mut env = ProcMacroEnv::new();
//!     env.add_path("string", string_path);
//!     env.process(tokens)
//! }
//! ```
//!
//! And used in the following manner:
//! ```ignore
//! string_concat! {
//!     let prefix = string::new("A ");
//!     let variable = "Variable";
//!     const VARIABLE: &str = prefix.concat(variable);
//!     const LITERAL: &str = prefix.concat("Literal");
//! }
//!
//! assert_eq!(VARIABLE, "A Variable");
//! assert_eq!(LITERAL, "A Literal");
//! ```
//!
//! Whilst these two examples are contrived, they serve to demonstrate the functionality of the
//! library. A more practical example would be to add functionality at compile time that is not
//! available on the target platform.
//!
//! For example, say the target platform is the microprocessor of an Arduino Uno (the atmega328p).
//! Code for this is usually compiled without the standard library and without Floating Point
//! arithmetic. If you wanted to make use (at compile time) of a library that uses both
//! floating points and the standard library, you can easily glue that functionality to a proc
//! macro using this library; and then use that proc macro to create a constant.
//!
//! A more concrete example is a constant array representing points of a sine wave at fixed intervals.
//! The calculations within the proc macro can make use of the standard library and floating points,
//! but the resulting constant array can be just u8s.
//!

extern crate proc_macro;

mod expression;
mod object;
mod object_type;
mod parameter;
mod path;
mod r#return;

pub use crate::expression::Expression;
pub use crate::object::Object;
pub use crate::object_type::{Method, MethodMut, ObjectType, SealedObjectType};
pub use crate::parameter::Parameter;
pub use crate::path::Function;
pub use crate::path::Path;
pub use crate::r#return::Return;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::ToTokens;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use syn::parse::{Error, Parser, Result as ParseResult};
use syn::spanned::Spanned;
use syn::{
    Block, Expr, ExprArray, ExprCall, ExprLit, ExprMacro, ExprMethodCall, ExprPath, ExprUnary,
    Ident, Item, ItemConst, Lit, LitInt, Local, LocalInit, Pat, PatIdent, Stmt, Type, TypeArray,
    UnOp,
};

/// A Result type that provides a Return enum within the Ok variant, and a String within the Err variant
pub type ReturnResult = Result<Return, String>;

type Dispatch = dyn Fn(Option<&str>, &str, &[Parameter]) -> ReturnResult;

type IdentifierMap = HashMap<String, Return>;

fn expr_iter_into_processed_return_slice<'a>(
    env: &ProcMacroEnv,
    identifiers: &IdentifierMap,
    args: impl Iterator<Item = &'a Expr>,
) -> ParseResult<Box<[Return]>> {
    let mut error = None;
    let returns: Box<[Return]> = args
        .map_while(|expr| match process_expr(env, identifiers, expr) {
            Err(err) => {
                let _ = error.insert(err);
                None
            }
            Ok(result) => Some(result),
        })
        .collect();
    match error {
        Some(err) => Err(err),
        None => Ok(returns),
    }
}

fn call_func<'a>(
    env: &ProcMacroEnv,
    identifiers: &IdentifierMap,
    function: &dyn Function,
    args: impl Iterator<Item = &'a Expr>,
    span: Span,
) -> ParseResult<Return> {
    let returns = expr_iter_into_processed_return_slice(env, identifiers, args)?;
    let mut error = None;
    let parameters: Box<[_]> = returns
        .iter()
        .map_while(|x| match x.as_parameter(identifiers) {
            Err(err) => {
                let _ = error.insert(err);
                None
            }
            Ok(value) => Some(value),
        })
        .collect();
    match error {
        Some(err) => Err(err),
        None => function
            .dispatch(&parameters)
            .map_err(|err| Error::new(span, err)),
    }
}

fn call_expr(
    env: &ProcMacroEnv,
    identifiers: &IdentifierMap,
    call: &ExprCall,
) -> ParseResult<Return> {
    let Expr::Path(ExprPath{path, qself: None, ..}) = call.func.as_ref() else {
        return Err(Error::new(call.span(), "only a simple path expression is supported for function calls"));
    };

    let len = path.segments.len();
    if len < 2 {
        return Err(Error::new(
            path.span(),
            "there are no functions under the root path",
        ));
    }
    let segments = &path.segments;
    let ident = &segments[0].ident;
    let Some(env_path) = env.get_path(ident) else {
        return Err(Error::new(path.span(), format!("'{ident}' is not available under this proc macro environment")));
    };

    let mut current_path = env_path;
    for i in 1..(len - 1) {
        let ident = &segments[i].ident;
        let Some(new_path) = current_path.get_path(&ident.to_string()) else {
            let path = path.to_token_stream().to_string();
            return Err(Error::new(ident.span(), format!("'{}' is not available under this proc macro environment", path.replace(' ', ""))));
        };
        current_path = new_path;
    }

    let ident = &segments[len - 1].ident;
    let Some(func) = env_path.get_function(&ident.to_string()) else {
        let path = path.to_token_stream().to_string();
        return Err(Error::new(ident.span(), format!("function '{ident}' is not available under this path ('{}') in this proc macro environment", path.replace(' ', ""))));
    };

    call_func(
        env,
        identifiers,
        func,
        call.args.iter(),
        call.paren_token.span.join(),
    )
}

fn method_call_func<'a>(
    env: &ProcMacroEnv,
    identifiers: &IdentifierMap,
    object: &Object,
    ident: &str,
    method: &str,
    args: impl Iterator<Item = &'a Expr>,
    span: Span,
) -> ParseResult<Return> {
    let returns = expr_iter_into_processed_return_slice(env, identifiers, args)?;
    let mut error = None;
    let parameters: Box<[_]> = returns
        .iter()
        .map_while(|x| match x.as_parameter(identifiers) {
            Err(err) => {
                let _ = error.insert(err);
                None
            }
            Ok(value) => Some(value),
        })
        .collect();

    match error {
        Some(err) => Err(err),
        None => object
            .call_ident(ident, method, &parameters)
            .map_err(|err| Error::new(span, err)),
    }
}

fn method_call_expr(
    env: &ProcMacroEnv,
    identifiers: &IdentifierMap,
    call: &ExprMethodCall,
) -> ParseResult<Return> {
    let Expr::Path(ExprPath{ref path, ..}) = *call.receiver else {
        return Err(Error::new(call.span(), "only simple identifiers are supported for method calls"));
    };

    if path.segments.len() != 1 {
        return Err(Error::new(
            call.span(),
            "only simple identifiers are supported for method calls",
        ));
    }

    let ident = &path.segments[0].ident;

    let Some(result) = identifiers.get(&ident.to_string()) else {
        return Err(Error::new(ident.span(), format!("'{ident}' has not been declared")));
    };

    let Return::Object(object) = result else {
        return Err(Error::new(ident.span(), format!("'{ident}' is not an object")));
    };

    let method = call.method.to_string();
    method_call_func(
        env,
        identifiers,
        object,
        &ident.to_string(),
        &method,
        call.args.iter(),
        call.paren_token.span.join(),
    )
}

fn process_expr(
    env: &ProcMacroEnv,
    identifiers: &IdentifierMap,
    expr: &Expr,
) -> ParseResult<Return> {
    Ok(match expr {
        Expr::Lit(ExprLit {
            lit: Lit::Str(string),
            ..
        }) => Return::String(string.value()),
        // Lit:Int is always unsigned, as negative numbers come through as a Unary
        Expr::Lit(ExprLit {
            lit: Lit::Int(uint),
            ..
        }) => Return::UInt(uint.base10_parse()?),
        Expr::Lit(ExprLit {
            lit: Lit::Float(float),
            ..
        }) => Return::Float(float.base10_parse()?),
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) if matches!(
            **expr,
            Expr::Lit(ExprLit {
                lit: Lit::Int(_),
                ..
            })
        ) =>
        {
            let Expr::Lit(ExprLit{lit: Lit::Int(ref int), ..}) = **expr else {
                panic!("BUG: match pattern for negative int needs fixing");
            };
            Return::Int(-int.base10_parse()?)
        }
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) if matches!(
            **expr,
            Expr::Lit(ExprLit {
                lit: Lit::Float(_),
                ..
            })
        ) =>
        {
            let Expr::Lit(ExprLit{lit: Lit::Float(ref float), ..}) = **expr else {
                panic!("BUG: match pattern for negative float needs fixing");
            };
            Return::Float(-float.base10_parse()?)
        }
        Expr::Call(call) => call_expr(env, identifiers, call)?,
        Expr::MethodCall(call) => method_call_expr(env, identifiers, call)?,
        Expr::Array(ExprArray { elems, .. }) => {
            let mut err = None;
            let result = elems
                .iter()
                .map_while(|expr| match process_expr(env, identifiers, expr) {
                    Ok(value) => Some(value),
                    Err(error) => {
                        let _ = err.insert(error);
                        None
                    }
                })
                .collect();
            match err {
                Some(err) => return Err(err),
                None => Return::Array(result),
            }
        }
        Expr::Path(ExprPath {
            path:
                syn::Path {
                    leading_colon: None,
                    segments,
                },
            ..
        }) => {
            if segments.len() != 1 {
                return Err(Error::new(
                    expr.span(),
                    "only a simple identifier is supported",
                ));
            }
            let ident = &segments[0].ident;
            let Some(value) = identifiers.get(&ident.to_string()) else {
                return Err(Error::new(ident.span(), format!("'{ident}' is not declared")));
            };

            match value {
                Return::UInt(uint) => Return::UInt(*uint),
                Return::Int(int) => Return::Int(*int),
                Return::Float(float) => Return::Float(*float),
                Return::Object(object) => Return::Object(object.clone()),
                Return::String(_) | Return::Array(_) | Return::Expression(_) => {
                    Return::Identifier(ident.to_string())
                } // late initialisation
                Return::Identifier(_) => {
                    panic!("An identifier should never be assigned an Identifier value")
                }
                Return::Unit => panic!("An identifier should never have the '()' value"),
                Return::Infer => {
                    return Err(Error::new(
                        expr.span(),
                        format!("'{ident}' has yet to infer it's value"),
                    ))
                }
            }
        }
        Expr::Infer(_) => Return::Infer,
        Expr::Macro(ExprMacro { ref mac, .. }) => {
            #[cfg(feature = "proc_macro_expand")]
            {
                let tokens = TokenStream::from(mac.into_token_stream())
                    .expand_expr()
                    .map_err(|err| Error::new(mac.span(), err.to_string()))?;
                let expr: Expr = syn::parse(tokens)?;
                return process_expr(env, identifiers, &expr);
            }
            #[cfg(not(feature = "proc_macro_expand"))]
            {
                let _ = mac; // suppress: warning: unused variable: `mac`
                return Err(Error::new(
                        expr.span(),
                        "Macro expansion is only supported when the 'proc_macro_expand' feature is enabled",
                    ));
            }
        }
        _ => {
            return Err(Error::new(
                expr.span(),
                format!(
                    "the following syntax is not supported: `{}`",
                    expr.to_token_stream()
                ),
            ))
        }
    })
}

fn get_identifiers(env: &ProcMacroEnv, statements: &Vec<Stmt>) -> ParseResult<IdentifierMap> {
    let mut identifiers = HashMap::new();
    for statement in statements {
        let result = match statement {
            Stmt::Local(Local {
                pat: Pat::Ident(PatIdent { ident, .. }),
                init: Some(LocalInit { expr, .. }),
                ..
            }) => Some((false, ident, None, expr)),
            Stmt::Item(Item::Const(ItemConst {
                ident, ty, expr, ..
            })) => Some((true, ident, Some(ty.as_ref()), expr)),
            Stmt::Expr(expr, _) => {
                process_expr(env, &identifiers, expr)?;
                None
            }
            _ => None,
        };
        // validation that the returned value is compatible with the declared type is left to 'rustc'
        if let Some((constant, ident, ty, expr)) = result {
            if env.has_path(ident) {
                return Err(Error::new(ident.span(), format!("'{ident}' can not be used as an identifier as it is defined as a path in this proc macro environment")));
            }

            let result = process_expr(env, &identifiers, expr)?;

            if matches!(result, Return::Unit) {
                return Err(Error::new(
                    expr.span(),
                    "'()' can not be used in an assignment",
                ));
            };

            if matches!(result, Return::Object(_) if constant) {
                return Err(Error::new(
                    expr.span(),
                    "constant can not be assigned an object",
                ));
            }

            if let Some(Type::Array(TypeArray {
                len:
                    Expr::Path(ExprPath {
                        qself: None,
                        path:
                            syn::Path {
                                leading_colon: None,
                                segments,
                            },
                        ..
                    }),
                ..
            })) = ty
            {
                let Return::Array(ref array) = result else {
                    return Err(Error::new(expr.span(), "expected an array"));
                };
                if segments.len() != 1 {
                    return Err(Error::new(
                        segments.span(),
                        "only a simple identifier is supported",
                    ));
                }
                let ident = &segments[0].ident;
                let Entry::Occupied(mut value) = identifiers.entry(ident.to_string()) else {
                    return Err(Error::new(ident.span(), format!("'{ident}' has not been declared")));
                };
                let value = value.get_mut();
                if matches!(value, Return::Infer) {
                    *value = Return::UInt(array.len() as u128);
                }
            }

            let Entry::Vacant(entry) = identifiers.entry(ident.to_string()) else {
                return Err(Error::new(ident.span(), format!("'{ident}' has already been defined")));
            };
            entry.insert(result);
        }
    }
    Ok(identifiers)
}

fn validate_expr(expr: &Expr) -> ParseResult<()> {
    match expr {
        Expr::Call(_) | Expr::MethodCall(_) => (), // these are allowed
        Expr::Lit(ExprLit { lit, .. })
            if matches!(lit, Lit::Str(_) | Lit::Int(_) | Lit::Float(_)) => {}
        Expr::Unary(ExprUnary {
            op: UnOp::Neg(_),
            expr,
            ..
        }) if matches!(**expr, Expr::Lit(ExprLit{ref lit, ..}) if matches!(lit, Lit::Int(_) | Lit::Float(_))) =>
            {}
        Expr::Array(_) => (), // TODO: can we further filter?
        Expr::Path(_) => (),  // TODO: can we further filter?
        Expr::Infer(_) => (),
        Expr::Macro(_) => {
            if !cfg!(feature = "proc_macro_expand") {
                return Err(Error::new(
                        expr.span(),
                        "Macro expansion is only supported when the 'proc_macro_expand' feature is enabled",
                    ));
            }
        }
        _ => {
            return Err(Error::new(
                expr.span(),
                "only literals and function/method calls are supported",
            ))
        }
    };

    Ok(())
}

fn validate_statements(statements: &Vec<Stmt>) -> ParseResult<()> {
    for statement in statements {
        match statement {
            Stmt::Local(local) => {
                if !matches!(
                    local.pat,
                    Pat::Ident(PatIdent {
                        by_ref: None,
                        mutability: None,
                        subpat: None,
                        ..
                    })
                ) {
                    return Err(Error::new(
                        local.pat.span(),
                        "only a simple, non-mutable, type implied identifier pattern is supported",
                    ));
                }

                let Some(ref init) = local.init else {
                    return Err(Error::new(local.span(), "all 'let' statements must have an initialiser"));
                };

                if let Some((_, diverge)) = &init.diverge {
                    return Err(Error::new(
                        diverge.span(),
                        "divergence on initialiser is not supported",
                    ));
                }

                validate_expr(&init.expr)?;
            }
            Stmt::Item(item) => {
                let Item::Const(item_const) = item else {
                    return Err(Error::new(item.span(), "only 'const' items are supported"));
                };

                let mut ty = item_const.ty.as_ref();
                while let Type::Array(array_type) = ty {
                    ty = array_type.elem.as_ref();
                }

                if let Type::Reference(ref_type) = ty {
                    ty = ref_type.elem.as_ref()
                };

                if !matches!(ty, Type::Path(_)) {
                    return Err(Error::new(ty.span(), "only simple types are supported"));
                }

                validate_expr(item_const.expr.as_ref())?;
            }
            Stmt::Expr(expr, Some(_)) => {
                validate_expr(expr)?;
            }
            Stmt::Expr(_, None) => {
                return Err(Error::new(
                    statement.span(),
                    "expected a semi-colon at the end of this expression",
                ));
            }
            Stmt::Macro(_) => {
                // macros will be passed through
            }
        }
    }

    Ok(())
}

/// The ProcMacroEnv struct is the root of the proc macro environment. It defines the Path
/// structure that is used for calling functions
pub struct ProcMacroEnv<'a> {
    paths: HashMap<Ident, Path<'a>>,
}

impl<'a> ProcMacroEnv<'a> {
    /// Creates a new proc macro environement. This would usually be called only once per proc
    /// macro definition.
    pub fn new() -> Self {
        let paths = HashMap::new();
        Self { paths }
    }

    /// Adds a path to the root of the proc macro environment.
    ///
    /// # Panics
    /// This method will panic if the same identifier is added twice or the identifier is invalid.
    pub fn add_path(&mut self, ident: &str, path: Path<'a>) {
        let ident = Ident::new(ident, Span::call_site());
        match self.paths.entry(ident) {
            Entry::Vacant(entry) => entry.insert(path),
            Entry::Occupied(entry) => panic!("proc macro environment is faulty: '{}' has been added as a root path multiple times", entry.key()),
        };
    }

    fn process_with_result(&self, tokens: TokenStream) -> ParseResult<TokenStream2> {
        let statements = Parser::parse(Block::parse_within, tokens)?;

        validate_statements(&statements)?;

        let identifiers = get_identifiers(self, &statements)?;

        let mut output = TokenStream2::new();
        for mut statement in statements
            .into_iter()
            .filter(|x| !matches!(x, Stmt::Local(_) | Stmt::Expr(..)))
        {
            if let Stmt::Item(Item::Const(ItemConst {
                ident,
                ref mut ty,
                ref mut expr,
                ..
            })) = &mut statement
            {
                if ident.to_string().starts_with('_') {
                    // exclude identifiers that start with an underscore
                    continue;
                }
                if !matches!(**expr, Expr::Lit(_) | Expr::Unary(_)) {
                    let value = identifiers
                        .get(&ident.to_string())
                        .expect("all identifiers should be declared by this point");
                    if matches!(value, Return::Infer) {
                        return Err(Error::new(ident.span(), format!("'{ident}' is never used. Needs to be used as an array length to determine value.")));
                    }
                    if let Type::Array(TypeArray {
                        len: ref mut len @ Expr::Infer(_),
                        ..
                    }) = ty.as_mut()
                    {
                        if let Return::Array(value) = value {
                            // validation that all elements of the array are the same type is left
                            // to 'rustc'
                            *len = Expr::Lit(ExprLit {
                                attrs: Vec::new(),
                                lit: Lit::Int(LitInt::new(&value.len().to_string(), len.span())),
                            });
                        };
                    }
                    *expr = Box::new(value.as_expr(&identifiers, expr.span()));
                }
            }

            statement.to_tokens(&mut output);
        }
        Ok(output)
    }

    /// Process the provided TokenSteam. On success, this will return a TokenSteam that defines a
    /// set of constants. On failure, a compile time error will be reported.
    pub fn process(&self, tokens: TokenStream) -> TokenStream {
        match self.process_with_result(tokens) {
            Ok(tokens) => tokens,
            Err(err) => err.to_compile_error(),
        }
        .into()
    }

    fn has_path(&self, ident: &Ident) -> bool {
        self.paths.contains_key(ident)
    }

    fn get_path(&self, ident: &Ident) -> Option<&Path> {
        self.paths.get(ident)
    }
}

impl Default for ProcMacroEnv<'_> {
    fn default() -> Self {
        Self::new()
    }
}
