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

#[derive(Debug)]
pub struct CommentError {
    pub msg: String,
}

pub fn strip_long_comments(input: &str) -> Result<String, CommentError> {
    if input.is_empty() {
        return Ok(input.to_string());
    }
    let mut chars = input.chars();
    let mut curr = chars.next().unwrap();
    let mut look = chars.next();

    let mut comment_depth: i32 = 0;
    let mut result = String::from("");
    loop {
        if curr == '(' {
            match look {
                Some('*') => {
                    comment_depth += 1;
                    (curr, look) = match chars.next() {
                        None => break,
                        Some(c) => (c, chars.next()),
                    }
                }
                Some(l) => {
                    if comment_depth == 0 {
                        result.push(curr);
                    }
                    (curr, look) = (l, chars.next());
                }
                None => {
                    if comment_depth == 0 {
                        result.push(curr);
                    };
                    break;
                }
            }
        } else if curr == '*' {
            match look {
                Some(')') => {
                    comment_depth -= 1;
                    (curr, look) = match chars.next() {
                        None => break,
                        Some(c) => (c, chars.next()),
                    }
                }
                Some(l) => {
                    if comment_depth == 0 {
                        result.push(curr);
                    }
                    (curr, look) = (l, chars.next());
                }
                None => {
                    if comment_depth == 0 {
                        result.push(curr);
                    };
                    break;
                }
            }
        } else {
            if comment_depth == 0 {
                result.push(curr);
            }
            (curr, look) = match look {
                None => break,
                Some(l) => (l, chars.next()),
            };
        };

        if comment_depth < 0 {
            let msg = "Encountered unmatched *)".to_string();
            return Err(CommentError { msg });
        }
    }
    if comment_depth != 0 {
        let msg = format!(
            "At least one unmatched (*.  (Comment depth was {} upon finishing scan.)",
            comment_depth
        );
        return Err(CommentError { msg });
    }
    Ok(result)
}

pub fn tokenize_all(input: &str) -> Vec<Token> {
    let stripped = &strip_long_comments(input).unwrap();
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
    fn strip_comments1() {
        let code: &str = "
        class Apple {};
        class Orange {};
        ";
        let result = strip_long_comments(code).unwrap();
        let desired = code.to_owned();
        assert_eq!(result, desired);
    }
    #[test]
    fn strip_comments2() {
        let code: &str = "xyz(*abc(*def*ghi(jkl*)*mno)*)p\nqr";
        let result = strip_long_comments(code).unwrap();
        let desired = "xyzp\nqr";
        assert_eq!(result, desired);
    }
    #[test]
    fn strip_comments3() {
        let code: &str = "xyz(((*a)bc**))def";
        let result = strip_long_comments(code).unwrap();
        let desired = "xyz(()def";
        assert_eq!(result, desired);
    }
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
