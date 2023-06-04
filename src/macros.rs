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
/// use dp::{rule_ref, rule, Pattern, UnderlyingRule};
///
/// rule!(struct Rule: "lol");
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
        $crate::Pattern::RuleReference(Box::new(<$rule as $crate::UnderlyingRule>::rule().into()))
    };
    ($name: expr) => {
        $crate::Pattern::RuleReference(Box::new($name.into()))
    };
}

/// Macro to simplify creation of sequences
#[macro_export(local_inner_macros)]
macro_rules! seq {
	// Hide distracting implementation details from the generated rustdoc.
	($($tokens:tt)+) => {
		seq_internal!($($tokens)+)
	};
}

#[macro_export]
#[doc(hidden)]
macro_rules! seq_internal {
	($expr: expr) => {
		$crate::Pattern::from($expr)
	};
	($($expr: expr),+) => {
		$crate::patterns::Sequence::from(
			vec![$($crate::Pattern::from($expr)),+]
		)
	};
	($($expr: expr),+ => $action: expr) => {
		$crate::patterns::Sequence::new(
			vec![$($crate::Pattern::from($expr)),+],
			$action
		)
	};
}

/// Macro for convenient rule creation
/// ```
/// # use pretty_assertions::assert_eq;
///
/// use dp::{rule, Rule, UnderlyingRule};
///
/// rule!(struct X: "lol");
/// assert_eq!(X::rule(), Rule::new("X", "lol"));
/// ```
#[macro_export]
macro_rules! rule {
    (struct $name:ident : $pattern:expr) => {
        pub struct $name;

        rule!($name : $pattern);
    };
    ($name:ty : $pattern:expr) => {
        impl $crate::UnderlyingRule for $name {
            fn rule() -> $crate::Rule {
                $crate::Rule::new(stringify!($name), $pattern)
            }
        }
    };
}
