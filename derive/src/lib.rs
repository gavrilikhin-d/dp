use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemStruct};

/// Examples:
/// ```
/// use derive::{rule, pattern};
///
/// #[rule("{username: 'a'} ':' {password: 'b'}")]
/// struct Credentials {
///     // #[pattern("")]
///     pub username: String,
///     // #[pattern("/[_0-9a-zA-Z]+/")]
///     pub password: String
/// }
///
/// let credentials: Credentials = "a:b".parse().unwrap();
/// assert_eq!(credentials.username, "a");
/// assert_eq!(credentials.password, "b");
/// ```
#[proc_macro_attribute]
pub fn rule(metadata: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemStruct);
    let metadata = metadata.to_string();

    let metadata: proc_macro2::TokenStream = metadata.trim_matches('"').parse().unwrap();

    let ident = &input.ident;

    let output = quote! {
        #[derive(serde::Serialize, serde::Deserialize)]
        #input

        dp::rule!(#ident: #metadata);

        impl std::str::FromStr for #ident {
            type Err = Vec<dp::errors::Error>;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                use dp::{parser::Parser, UnderlyingRule};

                let mut context = dp::Context::default();
                let mut res = Self::rule().parse(s, &mut context);
                if res.syntax.has_errors() {
                    return Err(res.syntax.errors().cloned().collect());
                }
                let value = res.ast.take().unwrap().get(Self::name()).unwrap().clone();
                println!("{}", serde_json::to_string_pretty(&value).unwrap());
                Ok(serde_json::from_value(value).unwrap())
            }
        }
    };
    println!("{}", output);
    output.into()
}

#[proc_macro_attribute]
pub fn pattern(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    let output = input;
    output.into()
}
