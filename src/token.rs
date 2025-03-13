// ### Lexing library. ###

use crate::token_utils::strip_long_comments_and_get_insertion_map;
use logos::{Lexer, Logos, SpannedIter};
use std::collections::HashSet;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteralError {
    msg: String,
}

fn str_const_callback(lex: &str) -> Result<String, StringLiteralError> {
    // 1) Check that literal begins and ends with quotes.
    // 2) Check that literal is under 1024 character.
    // 3) Make sure there are no Null characters or unescaped newlines
    // 4) Process all escaped characters.

    let lex = lex
        .strip_prefix("\"")
        .ok_or(StringLiteralError {
            msg: "Expected string literal to start with \"".to_owned(),
        })?
        .strip_suffix('\"')
        .ok_or(StringLiteralError {
            msg: "Expected string literal to end with \"".to_owned(),
        })?;

    // Check under maximum length.
    if lex.len() > 1024 {
        panic!("String constants may not be longer than 1024 characters.")
    }

    // Make sure there are no unescaped newlines, EOF, or \0
    let banned = HashSet::<char>::from(['\n', '\x00']);
    lex.chars().try_for_each(|c| {
        if banned.contains(&c) {
            let msg = format!("String constant {} contains illegal character {}", lex, c);
            Err(StringLiteralError { msg })
        } else {
            Ok(())
        }
    })?;

    // Process user escapes.
    let mut processed = String::from("");
    let mut chars = lex.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                None => break,
                Some('n') => processed.push('\n'),
                Some('b') => processed.push('\x08'),
                Some('t') => processed.push('\t'),
                Some('f') => processed.push('\x12'),
                Some(other) => processed.push(other),
            }
        } else {
            processed.push(c);
        }
    }
    Ok(processed)
}

fn error_callback(lex: &mut Lexer<Token>) -> String {
    let lex = lex.slice().to_string();
    format!("Error trying to tokenize at character {}", lex)
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f\r]+|--(.)*\n")] // Ignore Whitespace and double dash comments.
pub enum Token {
    // Special symbols
    #[regex(r"\(")]
    LParen,

    #[regex(r"\)")]
    RParen,

    #[regex(r"\{")]
    LCurly,

    #[regex(r"\}")]
    RCurly,

    #[regex(r":")]
    Colon,

    #[regex(r";")]
    Semicolon,

    #[regex(r"\.")]
    Period,

    #[regex(r",")]
    Comma,

    #[regex(r"<")]
    LT,

    #[regex(r"=")]
    Eq,

    #[regex(r"@")]
    At,

    #[regex(r"\+")]
    Plus,

    #[regex(r"-")]
    Minus,

    #[regex(r"\*")]
    Times,

    #[regex(r"/")]
    Divide,

    #[regex(r"\\")]
    Backslash,

    #[regex(r"<=")]
    LTE,

    #[regex(r"=>")]
    DArrow,

    #[regex(r"<-")]
    Assign,

    #[regex("~")]
    Comp,

    // Identifiers and Constants.
    #[regex("[A-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    TypeId(String),

    #[regex("[a-z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    ObjectId(String),

    #[regex("[0-9]+", |lex| lex.slice().to_string())]
    IntConst(String),

    #[regex("true|false", |value| value.slice() == "true")]
    BoolConst(bool),

    #[regex("\"[^\"\0]*\"", |lex| str_const_callback(lex.slice()))]
    StrConst(Result<String, StringLiteralError>),

    // Keywords.
    // TODO: in the spec these are actually case insensitive.
    #[regex("class")]
    Class,

    #[regex("else")]
    Else,

    #[regex("fi")]
    Fi,

    #[regex("if")]
    If,

    #[regex("in")]
    In,

    #[regex("inherits")]
    Inherits,

    #[regex("isvoid")]
    IsVoid,

    #[regex("let")]
    Let,

    #[regex("loop")]
    Loop,

    #[regex("pool")]
    Pool,

    #[regex("then")]
    Then,

    #[regex("while")]
    While,

    #[regex("case")]
    Case,

    #[regex("esac")]
    Esac,

    #[regex("new")]
    New,

    #[regex("of")]
    Of,

    #[regex("not")]
    Not,

    // Error
    #[regex(".", error_callback, priority = 1)]
    Error(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn tokenize_all(input: &str) -> Vec<Token> {
    // Not used with the parser, but useful for diagnostics.
    let stripped = &strip_long_comments_and_get_insertion_map(input).unwrap().0;
    let lexer = Token::lexer(stripped).spanned();
    let mut result = vec![];
    for inner in lexer {
        let tok = match inner {
            (Err(_), span) => panic!("Lexing error between bytes {} and {}", span.start, span.end),
            (Ok(raw), _) => raw,
        };
        result.push(tok);
    }
    result
}

#[cfg(test)]
mod token_tests {

    use super::*;

    #[test]
    fn str_const_callback1() {
        // Error:  Contains unescaped newline.
        let code: &str = "\"x\ny\"";
        let result = str_const_callback(code);
        assert!(result.is_err())
    }

    #[test]
    fn str_const_callback2() {
        // Error: Does not begin with quote.
        let code: &str = r#"blah""#;
        let result = str_const_callback(code);
        assert!(result.is_err())
    }

    #[test]
    fn str_const_callback3() {
        // Error: Does not end with quote.
        let code: &str = r#""blah"#;
        let result = str_const_callback(code);
        assert!(result.is_err())
    }

    #[test]
    fn str_const_callback4() {
        // Handle Escapes
        let code: &str = r#""a\nb\cd\te\ff\b""#;
        let result = str_const_callback(code).unwrap();
        let desired_result = "a\nbcd\te\x12f\x08".to_owned();
        assert_eq!(result, desired_result);
    }
}

// #######  For LALRPOP Parser #########
//
// lalrpop parser needs an iterator like that of CoolLexer below.
pub struct CoolLexer<'input> {
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> CoolLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexicalError {
    #[default]
    Other,
}

impl Iterator for CoolLexer<'_> {
    type Item = Result<(usize, Token, usize), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token_stream.next() {
            None => None,
            Some((Err(_), _)) => Some(Err(LexicalError::Other)),
            Some((Ok(token), span)) => Some(Ok((span.start, token, span.end))),
        }
    }
}
