// ### Token constants and lexing library. ###

use std::fmt;

use logos::{Lexer, Logos, SpannedIter};

fn str_const_callback(lex: &mut Lexer<Token>) -> String {
    let lex = lex.slice().to_string();
    // TODO! Check length and handle backslashes.
    lex
}

fn error_callback(lex: &mut Lexer<Token>) -> String {
    let lex = lex.slice().to_string();
    // TODO! Check length and handle backslashes.
    format!("Error trying to lex at character {}", lex)
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f\r]+|--(.)*\n")] // Ignore Whitespace and double dash comments.
                                        // TODO!:  UNHANDLED are multiline comments.  This might be harder in Logos than in Flex.  Might
                                        // Need to do a pre-pass just for multiline comments.
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
    EQ,

    #[regex(r"@")]
    AT,

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

    // Identifiers and Constants.
    // TODO!  Note that we should add these to as symbol table as some point.
    #[regex("[A-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    TypeId(String),

    #[regex("[a-z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    ObjectId(String),

    #[regex("[0-9]+", |lex| lex.slice().to_string())]
    IntConst(String),

    #[regex("true|false", |value| value.slice() == "true")]
    BoolConst(bool),

    #[regex("\"[^\"\0]*\"", str_const_callback)]
    StrConst(String),

    // Keywords (TODO (maybe): in the spec these are actually case insensitive.
    // I never liked this.  Maybe leave as is.
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
    let lexer = Token::lexer(input).spanned();
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

// #######  For LALRPOP Parser #########
//
// lalrpop parser needs an iterator like CoolLexer below.
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

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
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token_stream.next() {
            None => None,
            Some((Err(_), _)) => Some(Err(LexicalError::Other)),
            Some((Ok(token), span)) => Some(Ok((span.start, token, span.end))),
        }
    }
}

#[cfg(test)]
mod lex_tests {
    //use super::*;
}
