use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while1};
use nom::character::complete::{
    alpha1, alphanumeric0, anychar, char, digit1, line_ending, space0, space1,
};
use nom::combinator::{eof, opt, recognize};
use nom::multi::{many0, many1};
use nom::sequence::{pair, terminated, tuple};
use nom::{sequence::delimited, IResult};

use crate::CmdExpandError;

#[derive(Debug, PartialEq)]
pub(crate) enum CommandPart<'a> {
    Space(&'a str),
    Text(&'a str),
}

pub(crate) enum TextPart<'a> {
    StarPlaceHolder,
    AtPlaceHolder,
    NumberPlaceHolder(&'a str),
    VarPlaceHolder(&'a str),
    NormalText(&'a str),
}

fn escaped_char(input: &str) -> IResult<&str, &str> {
    recognize(pair(char('\\'), anychar))(input)
}

pub(crate) fn parse_command(input: &str) -> Result<Vec<CommandPart>, CmdExpandError> {
    if input.trim().is_empty() {
        return Ok(vec![CommandPart::Space(input)]);
    }

    fn double_quote_text(input: &str) -> IResult<&str, &str> {
        recognize(delimited(
            char('"'),
            many0(alt((is_not("\"\\"), escaped_char))),
            char('"'),
        ))(input)
    }

    fn single_quote_text(input: &str) -> IResult<&str, &str> {
        recognize(delimited(
            char('\''),
            many0(alt((is_not("'"), escaped_char))),
            char('\''),
        ))(input)
    }

    fn no_quote_text(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| !c.is_whitespace())(input)
    }

    fn one_line_cmd(input: &str) -> IResult<&str, Vec<CommandPart>> {
        let (input, (leading_space, command, args, trailing_space, line_end)) = tuple((
            space0,
            alt((double_quote_text, single_quote_text, no_quote_text)),
            many0(tuple((
                space1,
                alt((double_quote_text, single_quote_text, no_quote_text)),
            ))),
            space0,
            opt(line_ending),
        ))(input)?;

        let mut parts = Vec::new();
        if !leading_space.is_empty() {
            parts.push(CommandPart::Space(leading_space));
        }
        parts.push(CommandPart::Text(command));
        for (space, arg) in args {
            parts.push(CommandPart::Space(space));
            parts.push(CommandPart::Text(arg));
        }
        if !trailing_space.is_empty() {
            parts.push(CommandPart::Space(trailing_space));
        }
        if let Some(le) = line_end {
            parts.push(CommandPart::Space(le));
        }
        Ok((input, parts))
    }

    fn empty_lines(input: &str) -> IResult<&str, Vec<CommandPart>> {
        let (input, output) = recognize(many1(alt((space1, line_ending))))(input)?;
        Ok((input, vec![CommandPart::Space(output)]))
    }

    let (_, lines) = terminated(many1(alt((one_line_cmd, empty_lines))), eof)(input)
        .map_err(|_| CmdExpandError::ParseCmdError(input.to_string()))?;
    Ok(lines.into_iter().flatten().collect())
}

pub(crate) fn parse_text(input: &str) -> Result<Vec<TextPart>, CmdExpandError> {
    fn star_placeholder(input: &str) -> IResult<&str, TextPart> {
        let (input, _) = tag("%*")(input)?;
        Ok((input, TextPart::StarPlaceHolder))
    }

    fn at_placeholder(input: &str) -> IResult<&str, TextPart> {
        let (input, _) = tag("%@")(input)?;
        Ok((input, TextPart::AtPlaceHolder))
    }

    fn number_placeholder(input: &str) -> IResult<&str, TextPart> {
        let (input, output) = recognize(pair(char('%'), digit1))(input)?;
        Ok((input, TextPart::NumberPlaceHolder(output)))
    }

    fn variable_name(input: &str) -> IResult<&str, &str> {
        recognize(pair(
            alt((alpha1, tag("_"))),
            alt((alphanumeric0, tag("_"))),
        ))(input)
    }

    fn variable_placeholder(input: &str) -> IResult<&str, TextPart> {
        let (input, output) = recognize(tuple((char('%'), variable_name, char('%'))))(input)?;
        Ok((input, TextPart::VarPlaceHolder(output)))
    }

    fn normal_text(input: &str) -> IResult<&str, TextPart> {
        let (input, output) = recognize(many1(alt((escaped_char, is_not("\\%")))))(input)?;
        Ok((input, TextPart::NormalText(output)))
    }

    let (_, parts) = many0(alt((
        normal_text,
        star_placeholder,
        at_placeholder,
        number_placeholder,
        variable_placeholder,
    )))(input)
    .map_err(|_| CmdExpandError::ParseTextError(input.to_string()))?;
    Ok(parts)
}

pub(crate) fn parse_argument_number(s: &str) -> Result<usize, CmdExpandError> {
    Ok(s.strip_prefix('%')
        .ok_or(CmdExpandError::ParseArgumentError(s.to_string()))?
        .parse()?)
}

pub(crate) fn parse_variable(s: &str) -> Result<&str, CmdExpandError> {
    Ok(s.strip_prefix('%')
        .ok_or(CmdExpandError::ParseVariableError(s.to_string()))?
        .strip_suffix('%')
        .ok_or(CmdExpandError::ParseVariableError(s.to_string()))?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_command() {
        assert_eq!(parse_command("echo hello").unwrap().len(), 3);
        assert_eq!(parse_command("%EDITOR% somefile").unwrap().len(), 3);
    }
}
