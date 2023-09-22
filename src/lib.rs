mod expander;
mod parser;

pub use expander::env_context;
pub use expander::Expander;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CmdExpandError {
    #[error("cannot parse command `{0}`")]
    CmdParseError(String),
    #[error("cannot parse argument `{0}`")]
    ArgParseError(String),
}
