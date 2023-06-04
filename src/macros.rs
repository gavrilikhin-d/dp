/// Create text from literal or token
/// ```
/// # use pretty_assertions::assert_eq;
///
/// use dp::{text, Pattern};
///
/// assert_eq!(text!(,), ",".into());
/// assert_eq!(text!(lol), "lol".into());
/// assert_eq!(text!(")"), ")".into());
/// ```
#[macro_export]
macro_rules! text {
    ($text: literal) => {
        $crate::Pattern::Text($text.to_string())
    };
    ($text: tt) => {
        $crate::Pattern::Text(stringify!($text).to_string())
    };
}

/// Creates a rule reference
/// ```
/// # use pretty_assertions::assert_eq;
///
/// use dp::{rule_ref, rule, Pattern};
///
/// rule!(Rule: "lol");
/// assert_eq!(
/// 	rule_ref!(Rule),
/// 	Pattern::RuleReference(Box::new(Rule::rule().into()))
/// );
/// assert_eq!(
/// 	rule_ref!("Rule"),
/// 	Pattern::RuleReference(Box::new("Rule".into()))
/// );
/// ```
#[macro_export]
macro_rules! rule_ref {
    ($rule: ty) => {
        $crate::Pattern::RuleReference(Box::new(<$rule>::rule().into()))
    };
    ($name: expr) => {
        $crate::Pattern::RuleReference(Box::new($name.into()))
    };
}

/// Macro for convenient rule creation
/// ```
/// # use pretty_assertions::assert_eq;
///
/// use dp::{rule, Rule};
///
/// rule!(X: "lol");
/// assert_eq!(X::rule(), Rule::new("X", "lol"));
/// ```
#[macro_export]
macro_rules! rule {
    ($name:ident : $pattern:expr) => {
        pub struct $name;

        impl $name {
            /// Get rule with this name
            pub fn rule() -> $crate::Rule {
                $crate::Rule::new(stringify!($name), $pattern)
            }
        }
    };
}
