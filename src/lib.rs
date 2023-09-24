//!
//! This library performs batch-like expansions in strings, that is, to expand `%VAR%` into their values inside some context. It's like a Windows version of [shellexpand](https://!crates.io/crates/shellexpand).
//!
//! Aside from context variables, it can also expands number arguments (e.g `%1`, `%*`) syntax with a list of arguments provided by user.
//!
//! Unlike `shellexpand`, variables that do not exist will expand to empty string.
//!
//! # Usage
//! See example below on how to define a context. The library also provids a default `cmdexpand::env_context` which can expand environment variable.
//! ```rust
//! use cmdexpand::Expander;
//!
//! fn mycontext(s: &str) -> Option<String> {
//!    match s {
//!        "NAME" => Some(String::from("Alice")),
//!        "HOBBY" => Some(String::from("coding")),
//!        _ => None,
//!    }
//! }
//!
//! assert_eq!(Expander::new(r#"echo "%NAME%'s hobby is %HOBBY%""#)
//!               .add_context(&mycontext)
//!               .expand()
//!               .unwrap(),
//!            r#"echo "Alice's hobby is coding""#);
//! ```
//!
//! You can provide a list of arguments too.
//! ```rust
//! use cmdexpand::Expander;
//!
//! // arguments start from 1. `%0` will always expand to empty string
//! assert_eq!(Expander::new("del %1")
//!             .add_args(&["a.txt", "b.txt"])
//!             .expand()
//!             .unwrap(), "del a.txt");
//!
//! // `%*` behaves like `$*` on Linux
//! assert_eq!(Expander::new("del %*")
//!             .add_args(&["a.txt", "b.txt"])
//!             .expand()
//!             .unwrap(), r#"del "a.txt b.txt""#);
//!
//! // `%@` behaves like `$@` on Linux, even though technically there's no `%@` on Windows
//! assert_eq!(Expander::new("del %@")
//!             .add_args(&["a.txt", "b.txt"])
//!             .expand()
//!             .unwrap(), r#"del a.txt b.txt"#);
//! ```
mod expander;
mod parser;

pub use expander::env_context;
pub use expander::Context;
pub use expander::Expander;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CmdExpandError {
    #[error("cannot parse command `{0}`")]
    ParseCmdError(String),
    #[error("cannot parse argument `{0}`")]
    ParseTextError(String),
    #[error("wrong argument format `{0}`")]
    ParseArgumentError(String),
    #[error(transparent)]
    ParseIntError(#[from] std::num::ParseIntError),
}
