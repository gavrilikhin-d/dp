use darling::FromDeriveInput;
use syn::{parse_macro_input, DeriveInput};

#[derive(FromDeriveInput)]
#[darling(attributes(rule))]
struct RuleDeriveAttributes {
    pub ident: syn::Ident,
}

use quote::quote;

/// Examples:
/// ```
/// use derive::Rule;
///
/// #[derive(Rule)]
/// #[rule("{username}:{password}")]
/// struct Credentials {
/// 	#[pattern("/[_0-9a-zA-Z]+/")]
/// 	pub username: String,
///     #[pattern("/[_0-9a-zA-Z]+/")]
/// 	pub password: String
/// }
/// ```
#[proc_macro_derive(Rule, attributes(rule, pattern))]
pub fn rule(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    RuleDeriveAttributes::from_derive_input(&input)
        .map(|args| {
            let expanded = quote! {};
            proc_macro::TokenStream::from(expanded)
        })
        .unwrap_or_else(|e| e.write_errors().into())
}
