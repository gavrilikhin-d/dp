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
#[macro_export]
macro_rules! seq {
	($expr: expr) => {
		$crate::Pattern::from($expr)
	};
	($($expr: expr),+) => {
		$crate::patterns::Sequence::from(
			vec![$($crate::Pattern::from($expr)),+]
		)
	};
	($($expr: expr),+ => $($action:tt)+) => {
		$crate::patterns::Sequence::new(
			vec![$($crate::Pattern::from($expr)),+],
			$crate::action!($($action)+)
		)
	};
}

/// Macro to simplify creation of action
#[macro_export]
macro_rules! action {
    (throw $($expr:tt)+) => {
        $crate::Action::Throw($crate::expr!($($expr)+))
    };
    ($($expr:tt)+) => {
        $crate::Action::Return($crate::expr!($($expr)+))
    };
}

/// Macro to simplify creation of expressions
#[macro_export]
macro_rules! expr {
    ($var: ident) => {
        $crate::Expression::Variable(stringify!($var).to_string())
    };
    ($expr: expr) => {
        $crate::Expression::from($expr)
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

/// Macro to simplify creation of alternatives
#[macro_export]
macro_rules! alts {
    ($head: expr, $($tail: expr),+) => {
		$crate::Pattern::Alternatives(vec![$head.into(), $($tail.into()),+].into())
	};
}

#[macro_export]
macro_rules! obj {
    (@field $name:ident) => {
        crate::expressions::FieldInitializer {
            name: None,
            value: expr!($name),
        }
    };
    (@field $name:ident : $value:expr ) => {
        crate::expressions::FieldInitializer {
            name: Some(stringify!($name).to_string()),
            value: $value.into(),
        }
    };

    {$($name:ident $(: $value:expr)?),*} => {
        crate::expressions::ObjectConstructor {
            ty: None,
            initializers: vec![$(obj!(@field $name $(: $value)?)),*],
        }
    };
    ($ty:ident { $($name:ident $(: $value:expr)?),* }) => {
        crate::expressions::ObjectConstructor {
            ty: Some(stringify!($ty).to_string()),
            initializers: vec![$(obj!(@field $name $(: $value)?)),*],
        }
    };
}
