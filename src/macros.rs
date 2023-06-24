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
/// 	Pattern::RuleReference(Box::new("Rule".into()))
/// );
/// assert_eq!(
/// 	rule_ref!("Rule"),
/// 	Pattern::RuleReference(Box::new("Rule".into()))
/// );
/// ```
#[macro_export]
macro_rules! rule_ref {
    ($rule: ty) => {
        $crate::Pattern::RuleReference(Box::new(<$rule as $crate::UnderlyingRule>::name().into()))
    };
    ($name: expr) => {
        $crate::Pattern::RuleReference(Box::new($name.into()))
    };
}

/// Macro to simplify creation of sequences
/// ```
/// use dp::{seq, action, patterns::Sequence};
///
/// assert_eq!(
/// 	seq!('a' 'b' 'c'),
///     vec![
/// 		"a".into(), "b".into(), "c".into()
/// 	].into()
/// );
/// assert_eq!(
///     seq!('(' {value: "x"} ')'),
///     vec![
///       '('.into(),
///        ("value", "x").into(),
///        ')'.into()
///     ].into()
/// );
/// assert_eq!(
///     seq!('a' => 1),
///     Sequence::new(
///         vec!["a".into()].into(),
///         action!(1)
///     )
/// )
/// ```
#[macro_export]
macro_rules! seq {
	(@ [$($processed:tt)*] {$name:ident : $($pattern:tt)+} $($tail:tt)*) => {
		$crate::seq!(
			@
			[$($processed)* $crate::Pattern::from($crate::named!($name: $($pattern)+)),]
			$($tail)*
		)
	};

	(@ [$($processed:tt)*] {$rust:expr} $($tail:tt)*) => {
		$crate::seq!(
			@
			[$($processed)* $crate::Pattern::from($rust),]
			$($tail)*
		)
	};

	(@ [$($processed:tt)*] $text:literal $($tail:tt)*) => {
		$crate::seq!(
			@
			[$($processed)* $crate::Pattern::from($text),]
			$($tail)*
		)
	};

	(@ [$($processed:tt)*] => $($action:tt)+) => {
		$crate::patterns::Sequence::new(
			vec![$($processed)*],
			$crate::action!($($action)+)
		)
	};

	(@ [$($processed:tt)*] $rule:ident $($tail:tt)*) => {
		$crate::seq!(
			@
			[$($processed)* $crate::rule_ref!($rule),]
			$($tail)*
		)
	};

	(@ [$($processed:tt)*]) => {
		$crate::patterns::Sequence::from(
			vec![$($processed)*]
		)
	};

	($($tokens:tt)+) => {
		$crate::seq!(@ [] $($tokens)+)
	}
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
    (@) => {
        $crate::Expression::Variable("@".to_string())
    };
    (@$var:ident) => {
        $crate::Expression::Variable(format!("@{}", stringify!($var)))
    };
    ($var:ident) => {
        $crate::Expression::Variable(stringify!($var).to_string())
    };
    (($expr:expr) as $ty:ident) => {
        $expr.cast_to($crate::expr!($ty))
    };
    ($var:ident as $ty:ident) => {
        $crate::expr!($var).cast_to($crate::expr!($ty))
    };
    ($expr:expr) => {
        $crate::Expression::from($expr)
    };
}

/// Macro to simplify creation of expressions
#[macro_export]
macro_rules! cast {
    ($ty:ident($($tokens:tt)+)) => {
        $crate::expr!($($tokens)+).cast_to(stringify!($ty))
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
    (struct $name:ident : $($pattern:tt)+) => {
        pub struct $name;

        rule!($name : $($pattern)+);
    };
    ($name:ty : $($pattern:tt)+) => {
        impl $crate::UnderlyingRule for $name {
			fn name() -> &'static str {
				stringify!($name)
			}

            fn rule() -> $crate::Rule {
                $crate::Rule::new(Self::name(), $crate::seq!($($pattern)+))
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
    ($head:ident $(| $tail:ident)+) => {
		$crate::Pattern::Alternatives(
			vec![
				$crate::rule_ref!($head),
				$($crate::rule_ref!($tail)),+
			]
		)
	};
}

/// Macro for convenient creation of object constructors
/// ```
/// use dp::{obj, expr, expressions::{ObjectConstructor, Initializer, FieldInitializer}};
///
/// assert_eq!(
///    obj!{},
///    ObjectConstructor {
/// 	ty: None,
///     initializers: vec![]
///    }
/// );
/// assert_eq!(
///    obj!(Type {}),
///    ObjectConstructor {
/// 	ty: Some("Type".to_string()),
///     initializers: vec![]
///    }
/// );
/// assert_eq!(
///    obj!(Type { ...a, b, c: 1}),
///    ObjectConstructor {
/// 	ty: Some("Type".to_string()),
///     initializers: vec![
///         Initializer::Expand(expr!(a)),
///         FieldInitializer {
///           name: None,
///           value: expr!(b)
///         }.into(),
///         FieldInitializer {
///           name: Some("c".to_string()),
///           value: expr!(1)
///         }.into(),
///     ]
///    }
/// )
/// ```
#[macro_export]
macro_rules! obj {
    (@field $name:ident) => {
        $crate::expressions::FieldInitializer {
            name: None,
            value: $crate::expr!($name),
        }.into()
    };
    (@field $name:ident : $value:expr ) => {
        $crate::expressions::FieldInitializer {
            name: Some(stringify!($name).to_string()),
            value: $value.into(),
        }.into()
    };

    (@expand $name:ident ) => {
        $crate::expressions::Initializer::Expand($crate::expr!($name))
    };

    {$(...$expand:ident),* $(,)? $($name:ident $(: $value:expr)?),*} => {
        $crate::expressions::ObjectConstructor {
            ty: None,
            initializers: vec![
				$(obj!(@expand $expand),)*
				$(obj!(@field $name $(: $value)?)),*
			],
        }
    };
    ($ty:ident {$(...$expand:ident),* $(,)? $($name:ident $(: $value:expr)?),* }) => {
        $crate::expressions::ObjectConstructor {
            ty: Some(stringify!($ty).to_string()),
            initializers: vec![
				$(obj!(@expand $expand),)*
				$(obj!(@field $name $(: $value)?)),*
			],
        }
    };
}

/// Macro to simplify creation of [`Named`] patterns
#[macro_export]
macro_rules! named {
    ($name:ident : $rule:ident) => {
        $crate::patterns::Named::from((stringify!($name), $crate::rule_ref!($rule)))
    };
    ($name:ident : $expr:expr) => {
        $crate::patterns::Named::from((stringify!($name), $expr))
    };
}

/// Macro to simplify creation of [`ArrayConstructor`]
/// ```
/// use dp::{arr, expr, expressions::{ArrayConstructor, ArrayElement}};
/// assert_eq!(
///    arr![],
///    ArrayConstructor(vec![])
/// );
/// assert_eq!(
///   arr![1, ...b, c],
///   ArrayConstructor(vec![
/// 	expr!(1).into(),
/// 	ArrayElement::Expand(expr!(b)),
/// 	expr!(c).into()
///   ])
/// );
/// ```
#[macro_export]
macro_rules! arr {
	(@elem ... $name:ident)  => {
		$crate::expressions::ArrayElement::Expand($crate::expr!($name))
	};

	(@elem $($tokens:tt)+)  => {
		$crate::expressions::ArrayElement::from($crate::expr!($($tokens)+))
	};

	[@ [$($processed:tt)*] [$($current:tt)+]; , $($tail:tt)*] => {
		$crate::arr![@ [$($processed)*  $crate::arr!(@elem $($current)+),] []; $($tail)*]
	};

	[@ [$($processed:tt)*] [$($current:tt)*]; $token:tt $($tail:tt)*] => {
		$crate::arr![@ [$($processed)*] [$($current)* $token]; $($tail)*]
	};

	[@ [$($processed:tt)*] [$($current:tt)+]; ] => {
		$crate::arr![@ [$($processed)*  $crate::arr!(@elem $($current)+),] [];]
	};

	[@ [$($processed:tt)*] []; ] => {
		$crate::expressions::ArrayConstructor(
			vec![
				$($processed)*
			]
		)
	};

	[] => {
		$crate::expressions::ArrayConstructor(vec![])
	};
	[$($tokens:tt)+] => {
		$crate::arr![@ [] []; $($tokens)+]
	};
}
