use crate::{
    parser::{parse_command, parse_text, CommandPart, TextPart},
    CmdExpandError,
};

pub type Context = dyn Fn(&str) -> Option<String>;

/// Configuration for expanding command
#[derive(Default)]
pub struct Expander<'a> {
    cmd_str: &'a str,
    contexts: Vec<&'a Context>,
    args: Vec<&'a str>,
    no_context: bool,
    no_positional_args: bool,
}

#[derive(Debug, Clone, Copy)]
enum QuoteChar {
    None,
    SingleQuote,
    DoubleQuote,
}

impl<'a> Expander<'a> {
    /// Create a new [Expander](Expander).
    pub fn new(cmd: &'a str) -> Self {
        Self {
            cmd_str: cmd,
            ..Default::default()
        }
    }

    /// Add a context. User can add multiple contexts by calling this function multiple times.
    /// If a variable exists in more than one context, the first found will be expanded.
    pub fn add_context(mut self, context: &'a Context) -> Self {
        self.contexts.push(context);
        self
    }

    /// Add an argument.
    pub fn add_arg(mut self, arg: &'a str) -> Self {
        self.args.push(arg);
        self
    }

    /// Add a list of arguments.
    pub fn add_args<T>(mut self, args: &'a [T]) -> Self
    where
        T: AsRef<str>,
    {
        for arg in args {
            self.args.push(arg.as_ref());
        }
        self
    }

    /// Does not expand context variables.
    /// ```rust
    /// use cmdexpand::Expander;
    ///
    /// // because `%HOME%` does not exist, it will be expanded to empty string
    /// assert_eq!(Expander::new("echo %HOME%").expand().unwrap(), "echo ");
    ///
    /// // with `disable_context`, `%HOME%` will stay the same
    /// assert_eq!(Expander::new("echo %HOME%").disable_context(true).expand().unwrap(), "echo %HOME%");
    /// ```
    pub fn disable_context(mut self, yes: bool) -> Self {
        self.no_context = yes;
        self
    }

    /// Does not expand positional arguments.
    /// ```rust
    /// use cmdexpand::Expander;
    ///
    /// // because `%1` does not exist, it will be expanded to empty string
    /// assert_eq!(Expander::new("echo %1").expand().unwrap(), "echo ");
    ///
    /// // with `disable_positional_aruguments`, `%1` will stay the same
    /// assert_eq!(Expander::new("echo %1").disable_positional_aruguments(true).expand().unwrap(), "echo %1");
    /// ```
    pub fn disable_positional_aruguments(mut self, yes: bool) -> Self {
        self.no_positional_args = yes;
        self
    }

    /// Expand the command string
    pub fn expand(&self) -> Result<String, CmdExpandError> {
        let parts = parse_command(self.cmd_str)?;
        let mut expanded_cmd = String::new();
        for part in parts {
            match part {
                CommandPart::Space(s) => expanded_cmd.push_str(s),
                CommandPart::Text(s) => {
                    expanded_cmd.push_str(&self.expand_text(s)?);
                }
            }
        }
        Ok(expanded_cmd)
    }

    fn expand_text(&self, arg: &str) -> Result<String, CmdExpandError> {
        let quote_char = if arg.starts_with('\'') {
            QuoteChar::SingleQuote
        } else if arg.starts_with('"') {
            QuoteChar::DoubleQuote
        } else {
            QuoteChar::None
        };

        let parts = parse_text(arg)?;
        let mut expanded_arg = String::new();
        for part in parts {
            match part {
                TextPart::NumberPlaceHolder(i) => {
                    if self.no_positional_args {
                        expanded_arg.push_str(&format!("%{i}"));
                    } else if i > 0 {
                        let content: &str = self.args.get(i - 1).copied().unwrap_or_default();
                        let replace_text = Expander::preprocess_content(content, quote_char);
                        expanded_arg.push_str(&replace_text);
                    }
                }
                TextPart::VarPlaceHolder(var) => {
                    if self.no_context {
                        expanded_arg.push_str(&format!("%{var}%"));
                    } else {
                        for &context in self.contexts.iter() {
                            if let Some(value) = context(var) {
                                let replace_text = Expander::preprocess_content(&value, quote_char);
                                expanded_arg.push_str(&replace_text);
                                break;
                            }
                        }
                    }
                }
                TextPart::NormalText(s) => expanded_arg.push_str(s),
                TextPart::StarPlaceHolder => {
                    if self.no_positional_args {
                        expanded_arg.push_str("%*")
                    } else {
                        let one_arg = self.args.join(" ");
                        let replace_text = Expander::preprocess_content(&one_arg, quote_char);
                        expanded_arg.push_str(&replace_text);
                    }
                }
                TextPart::AtPlaceHolder => {
                    if self.no_positional_args {
                        expanded_arg.push_str("%@")
                    } else {
                        for (i, &arg) in self.args.iter().enumerate() {
                            let replace_text = Expander::preprocess_content(arg, quote_char);
                            expanded_arg.push_str(&replace_text);
                            if i + 1 != self.args.len() {
                                expanded_arg.push(' ');
                            }
                        }
                    }
                }
            }
        }
        Ok(expanded_arg)
    }

    fn preprocess_content(content: &str, surrounding: QuoteChar) -> String {
        let inner_space = content.chars().any(|c| c.is_whitespace());
        match (surrounding, inner_space) {
            (QuoteChar::None, true) => format!("\"{}\"", content.replace('"', "\\\"")),
            (QuoteChar::None, false) => content.to_string(),
            (QuoteChar::SingleQuote, _) => content.replace('\'', "\\'").to_string(),
            (QuoteChar::DoubleQuote, _) => content.replace('"', "\\\"").to_string(),
        }
    }
}

/// Context that contains environment variables
pub fn env_context(var: &str) -> Option<String> {
    std::env::var(var).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_context() {
        if let Ok(value) = std::env::var("EDITOR") {
            assert_eq!(
                Expander::new("%EDITOR% somefile")
                    .add_context(&env_context)
                    .expand()
                    .unwrap(),
                format!("{value} somefile")
            );
        }
    }

    #[test]
    fn test_args_context() {
        assert_eq!(
            Expander::new("echo %1").add_arg("abc").expand().unwrap(),
            "echo abc"
        );
        assert_eq!(
            Expander::new("echo %2")
                .add_arg("abc")
                .add_arg("def")
                .expand()
                .unwrap(),
            "echo def"
        );
        assert_eq!(
            Expander::new("echo %1")
                .add_arg("abc def")
                .expand()
                .unwrap(),
            "echo \"abc def\""
        );
        assert_eq!(
            Expander::new("echo %1")
                .add_arg("\"abc\"")
                .expand()
                .unwrap(),
            "echo \"abc\""
        );
        assert_eq!(
            Expander::new(r#"echo "hello %1""#)
                .add_arg(r#""world""#)
                .expand()
                .unwrap(),
            r#"echo "hello \"world\"""#
        );
        assert_eq!(
            Expander::new(r#"echo "hello %1""#)
                .add_arg(r#""my king""#)
                .expand()
                .unwrap(),
            r#"echo "hello \"my king\"""#
        );
        assert_eq!(
            Expander::new(r#"echo "hello %1""#)
                .add_arg("my king")
                .expand()
                .unwrap(),
            r#"echo "hello my king""#
        );
        assert_eq!(
            Expander::new(r#"echo %1"#)
                .add_arg("cmd /C \"run something\"")
                .expand()
                .unwrap(),
            r#"echo "cmd /C \"run something\"""#
        );
        assert_eq!(
            Expander::new("cmd %@")
                .add_arg("abc")
                .add_arg("def ghk")
                .expand()
                .unwrap(),
            r#"cmd abc "def ghk""#
        );
        assert_eq!(
            Expander::new("cmd a\\%@")
                .add_arg("abc")
                .add_arg("def ghk")
                .expand()
                .unwrap(),
            r#"cmd a\%@"#
        );
        assert_eq!(
            Expander::new(r#"cmd "Hello \"world"#).expand().unwrap(),
            r#"cmd "Hello \"world"#
        );
        assert_eq!(
            Expander::new(r#"  a "b \"%1\""  "#)
                .add_args(&["c", "d"])
                .expand()
                .unwrap(),
            r#"  a "b \"c\""  "#
        );
    }
}
