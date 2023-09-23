use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while1};
use nom::character::complete::{alpha1, alphanumeric0, char, digit0, one_of, space0, space1};
use nom::combinator::{eof, recognize};
use nom::multi::{many0, many1};
use nom::sequence::{pair, preceded, tuple};
use nom::{sequence::delimited, IResult};

use crate::CmdExpandError;

#[derive(Debug, PartialEq)]
pub(crate) enum CommandPart<'a> {
    Space(&'a str),
    Command(&'a str),
    Argument(&'a str),
}

pub(crate) enum ArgumentPart<'a> {
    StarSymbolArgument,
    AtSymbolArgument,
    PositionalPlaceHolder(usize),
    VarPlaceHolder(&'a str),
    Other(&'a str),
}

fn double_quote_argument(input: &str) -> IResult<&str, &str> {
    recognize(delimited(char('"'), is_not("\""), char('"')))(input)
}

fn single_quote_argument(input: &str) -> IResult<&str, &str> {
    recognize(delimited(char('\''), is_not("'"), char('\'')))(input)
}

fn no_quote_argument(input: &str) -> IResult<&str, &str> {
    take_while1(|c: char| !c.is_whitespace())(input)
}

pub(crate) fn parse_command(input: &str) -> Result<Vec<CommandPart>, CmdExpandError> {
    let (_, (leading_space, command, args, trailing_space, _)) = tuple((
        space0,
        alt((
            double_quote_argument,
            single_quote_argument,
            no_quote_argument,
        )),
        many1(tuple((
            space1,
            alt((
                double_quote_argument,
                single_quote_argument,
                no_quote_argument,
            )),
        ))),
        space0,
        eof,
    ))(input)
    .map_err(|_| CmdExpandError::CmdParseError(input.to_string()))?;
    let mut parts = Vec::new();
    if !leading_space.is_empty() {
        parts.push(CommandPart::Space(leading_space));
    }
    parts.push(CommandPart::Command(command));
    for (space, arg) in args {
        parts.push(CommandPart::Space(space));
        parts.push(CommandPart::Argument(arg));
    }
    if !trailing_space.is_empty() {
        parts.push(CommandPart::Space(trailing_space));
    }
    Ok(parts)
}

fn number_greater_than_zero(input: &str) -> IResult<&str, &str> {
    recognize(pair(one_of("123456789"), digit0))(input)
}

fn star_symbol_argument(input: &str) -> IResult<&str, ArgumentPart> {
    let (input, _) = tag("%*")(input)?;
    Ok((input, ArgumentPart::StarSymbolArgument))
}

fn at_symbol_argument(input: &str) -> IResult<&str, ArgumentPart> {
    let (input, _) = tag("%@")(input)?;
    Ok((input, ArgumentPart::AtSymbolArgument))
}

fn positional_placeholder(input: &str) -> IResult<&str, ArgumentPart> {
    let (input, output) = preceded(char('%'), number_greater_than_zero)(input)?;
    let num: usize = output.parse().unwrap();
    Ok((input, ArgumentPart::PositionalPlaceHolder(num)))
}

fn variable_name(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        alt((alphanumeric0, tag("_"))),
    ))(input)
}

fn variable_placeholder(input: &str) -> IResult<&str, ArgumentPart> {
    let (input, output) = delimited(char('%'), variable_name, char('%'))(input)?;
    Ok((input, ArgumentPart::VarPlaceHolder(output)))
}

fn other(input: &str) -> IResult<&str, ArgumentPart> {
    let (input, output) = alt((tag("\\%"), is_not("%")))(input)?;
    Ok((input, ArgumentPart::Other(output)))
}

pub(crate) fn parse_argument(input: &str) -> Result<Vec<ArgumentPart>, CmdExpandError> {
    let (_, parts) = many0(alt((
        other,
        star_symbol_argument,
        at_symbol_argument,
        positional_placeholder,
        variable_placeholder,
    )))(input)
    .map_err(|_| CmdExpandError::ArgParseError(input.to_string()))?;
    Ok(parts)
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
