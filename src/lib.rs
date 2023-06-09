#![feature(let_chains)]
#![feature(lazy_cell)]

pub(crate) mod log;

pub mod action;
pub use action::Action;

pub mod syntax;

pub mod patterns;
pub use patterns::Pattern;

mod rule;
pub use rule::*;

pub mod parser;

mod context;
pub use context::*;

pub mod errors;

pub mod bootstrap;

pub mod expressions;
pub use expressions::Expression;

pub mod macros;

#[cfg(test)]
#[ctor::ctor]
fn init() {
    pretty_env_logger::init();
}
