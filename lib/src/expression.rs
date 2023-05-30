use proc_macro::TokenStream;
use syn::Expr;

/// The Expression struct contains a Rust expression that can be used to define a value for an
/// exported constant. The struct is created by converting a TokenStream using the
/// `Expression::try_from` function.
#[derive(Clone)]
pub struct Expression(pub(super) Expr);

impl TryFrom<TokenStream> for Expression {
    type Error = String;

    fn try_from(value: TokenStream) -> Result<Self, Self::Error> {
        Ok(Self(syn::parse(value).map_err(|err| err.to_string())?))
    }
}
