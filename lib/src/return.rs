use crate::{Expression, IdentifierMap, Object, Parameter};
use proc_macro2::Span;
use syn::parse::{Error, Result};
use syn::punctuated::Punctuated;
use syn::{Expr, ExprArray, ExprLit, ExprUnary, Lit, LitFloat, LitInt, LitStr, UnOp};

/// The Return enum is used as the return type of both methods and functions. It defines both the
/// type and the value.
pub enum Return {
    /// The 'Infer' variant is the same as the `_` token in Rust
    Infer,
    /// A signed integer
    Int(i128),
    /// An unsigned integer
    UInt(u128),
    /// A floating point value
    Float(f64),
    /// A String
    String(String),
    /// An Object (created via the `SealedObjectType::new_instance` method)
    Object(Object),
    /// An Expression (created via the `Expression::try_from::<TokenStream>` method)
    Expression(Expression),
    /// Can be used to indirectly reference another value within the proc macro environment
    Identifier(String),
    /// An array of Array enum values
    Array(Box<[Return]>),
    /// The 'Unit' variant is the same as the `()` token in Rust
    Unit,
}

impl Return {
    pub(super) fn as_parameter<'a>(&'a self, identifiers: &'a IdentifierMap) -> Result<Parameter> {
        Ok(match self {
            Return::String(string) => Parameter::String(string.clone()),
            Return::UInt(uint) => Parameter::UInt(*uint),
            Return::Int(int) => Parameter::Int(*int),
            Return::Float(float) => Parameter::Float(*float),
            Return::Array(array) => {
                let mut error = None;
                let array = Parameter::Array(
                    array
                        .iter()
                        .map_while(|x| match x.as_parameter(identifiers) {
                            Err(err) => {
                                let _ = error.insert(err);
                                None
                            }
                            Ok(value) => Some(value),
                        })
                        .collect(),
                );
                match error {
                    None => array,
                    Some(err) => return Err(err),
                }
            }
            Return::Object(object) => Parameter::Object(object.clone()),
            Return::Identifier(ident) => {
                let Some(value) = identifiers.get(ident) else {
                    return Err(Error::new(Span::call_site(), format!("'{ident}' is not declared")));
                };
                value.as_parameter(identifiers)?
            }
            Return::Expression(_) => {
                return Err(Error::new(
                    Span::call_site(),
                    "can't use an Expression as a parameter",
                ));
            }
            Return::Unit => {
                return Err(Error::new(
                    Span::call_site(),
                    "can't use '()' (Unit) as a parameter",
                ));
            }
            Return::Infer => {
                return Err(Error::new(
                    Span::call_site(),
                    "can't use '_' (Infer) as a parameter",
                ));
            }
        })
    }

    pub(super) fn as_expr(&self, identifiers: &IdentifierMap, span: Span) -> Expr {
        if let Return::Identifier(ident) = self {
            identifiers
                .get(ident)
                .expect("identifiers should all be declared by now")
                .as_expr(identifiers, Span::call_site())
        } else if let Return::Array(array) = self {
            let mut elems = Punctuated::new();
            for param in array.iter() {
                elems.push(param.as_expr(identifiers, span));
            }
            Expr::Array(ExprArray {
                attrs: Vec::new(),
                bracket_token: Default::default(),
                elems,
            })
        } else if let Return::Expression(expr) = self {
            expr.0.clone()
        } else {
            let lit = Expr::Lit(ExprLit {
                attrs: Vec::new(),
                lit: match self {
                    Return::String(string) => Lit::Str(LitStr::new(string, span)),
                    Return::UInt(uint) => Lit::Int(LitInt::new(&uint.to_string(), span)),
                    Return::Int(int) => Lit::Int(LitInt::new(&int.abs().to_string(), span)),
                    Return::Float(float) => {
                        Lit::Float(LitFloat::new(&float.abs().to_string(), span))
                    }
                    Return::Object(_) | Return::Unit => {
                        panic!("BUG: can't assign Object or '()' to a const")
                    }
                    Return::Infer => {
                        panic!("BUG: inferred values should be validated before here")
                    }
                    Return::Array(_) | Return::Identifier(_) | Return::Expression(_) => {
                        unreachable!("these are handled in the if and else ifs above")
                    }
                },
            });
            if matches!(self, Return::Int(int) if *int < 0)
                || matches!(self, Return::Float(float) if *float < 0.0)
            {
                Expr::Unary(ExprUnary {
                    attrs: Vec::new(),
                    op: UnOp::Neg(Default::default()),
                    expr: Box::new(lit),
                })
            } else {
                lit
            }
        }
    }
}

// This is required to avoid recursive conversion in Return::from on a slice
trait RecurseSafeFrom: Into<Return> + Clone {
    fn safe_into(self) -> Return {
        self.into()
    }
}

impl From<()> for Return {
    fn from(_: ()) -> Self {
        Return::Unit
    }
}

impl From<i8> for Return {
    fn from(value: i8) -> Self {
        Return::Int(value as i128)
    }
}

impl From<i16> for Return {
    fn from(value: i16) -> Self {
        Return::Int(value as i128)
    }
}

impl From<i32> for Return {
    fn from(value: i32) -> Self {
        Return::Int(value as i128)
    }
}

impl From<i64> for Return {
    fn from(value: i64) -> Self {
        Return::Int(value as i128)
    }
}

impl From<i128> for Return {
    fn from(value: i128) -> Self {
        Return::Int(value)
    }
}

impl From<isize> for Return {
    fn from(value: isize) -> Self {
        Return::UInt(value as u128)
    }
}

impl From<u8> for Return {
    fn from(value: u8) -> Self {
        Return::UInt(value as u128)
    }
}

impl From<u16> for Return {
    fn from(value: u16) -> Self {
        Return::UInt(value as u128)
    }
}

impl From<u32> for Return {
    fn from(value: u32) -> Self {
        Return::UInt(value as u128)
    }
}

impl From<u64> for Return {
    fn from(value: u64) -> Self {
        Return::UInt(value as u128)
    }
}

impl From<u128> for Return {
    fn from(value: u128) -> Self {
        Return::UInt(value)
    }
}

impl From<usize> for Return {
    fn from(value: usize) -> Self {
        Return::UInt(value as u128)
    }
}

impl From<f32> for Return {
    fn from(value: f32) -> Self {
        Return::Float(value as f64)
    }
}

impl From<f64> for Return {
    fn from(value: f64) -> Self {
        Return::Float(value)
    }
}

impl From<Object> for Return {
    fn from(value: Object) -> Self {
        Return::Object(value)
    }
}

impl From<Expression> for Return {
    fn from(value: Expression) -> Self {
        Return::Expression(value)
    }
}

impl From<String> for Return {
    fn from(value: String) -> Self {
        Return::String(value)
    }
}

impl RecurseSafeFrom for i8 {}
impl RecurseSafeFrom for i16 {}
impl RecurseSafeFrom for i32 {}
impl RecurseSafeFrom for i64 {}
impl RecurseSafeFrom for i128 {}
impl RecurseSafeFrom for isize {}
impl RecurseSafeFrom for u8 {}
impl RecurseSafeFrom for u16 {}
impl RecurseSafeFrom for u32 {}
impl RecurseSafeFrom for u64 {}
impl RecurseSafeFrom for u128 {}
impl RecurseSafeFrom for usize {}
impl RecurseSafeFrom for f32 {}
impl RecurseSafeFrom for f64 {}
impl RecurseSafeFrom for Object {}
impl RecurseSafeFrom for Expression {}
impl RecurseSafeFrom for String {}
impl RecurseSafeFrom for () {}

impl<T> From<Box<[T]>> for Return
where
    T: RecurseSafeFrom,
{
    fn from(value: Box<[T]>) -> Self {
        Self::from(&*value)
    }
}

impl<T> From<&[T]> for Return
where
    T: RecurseSafeFrom,
{
    fn from(value: &[T]) -> Self {
        Return::Array(value.iter().map(|x| x.clone().safe_into()).collect())
    }
}
