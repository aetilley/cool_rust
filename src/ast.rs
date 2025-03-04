use crate::token::{CoolLexer, LexicalError, Token};
use crate::cool_grammar::ProgramTyParser;

use lalrpop_util::ParseError;

#[derive(Debug)]
pub struct Program {
    classes: Classes
}

pub fn mk_program(cls: Classes) -> Program {
    Program {classes: cls}
}

pub type Classes = Vec<Class>;

#[derive(Clone, Debug)]
pub struct Class {}

pub fn mk_class() -> Class {Class{}}

pub fn parse(input: &str) -> Result<Program, ParseError<usize, Token, LexicalError>> {
    let lexer = CoolLexer::new(input);
    let parser = ProgramTyParser::new();
    parser.parse(lexer)
}
