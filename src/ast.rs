use std::fmt::Debug;

use crate::token::{CoolLexer, LexicalError, Token};
use crate::cool_grammar::ProgramTyParser;

use lalrpop_util::ParseError;


#[derive(Debug)]
pub struct Program {
    classes: Classes
}
impl Program {
    pub fn program(classes: Classes) -> Program {
        Program{classes}
    }
}


#[derive(Clone, Debug)]
pub struct Class {
    name: String,
    features: Features,
}
pub type Classes = Vec<Class>;

impl Class {
    pub fn class(name: String, features: Features) -> Class {
        Class{name, features}
    }
}

#[derive(Clone, Debug)]
pub struct Formal {
    name: String,
    typ: String,
}
pub type Formals = Vec<Formal>;
impl Formal {
    pub fn formal(name: String, typ: String) -> Formal {
        Formal{name, typ}
    }
}

#[derive(Clone, Debug)]
pub enum Feature {
    Attr{name: String, typ: String, init: Expr},
    Method{name: String, formals: Formals, typ: String, body: Expr},
}
pub type Features = Vec<Feature>;

impl Feature {
    pub fn attr(name: String, typ: String, init: Expr) -> Feature {
        Feature::Attr{name, typ, init}
    }
    pub fn method(name: String, formals: Formals, typ: String, body: Expr) -> Feature {
        Feature::Method{name, formals, typ, body}
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    NoExpr,
    Object(String),
    BoolConst(bool),
}

impl Expr {
    pub fn no_expr() -> Expr {
        Expr::NoExpr
    }

    pub fn object(name: String) -> Expr {
        Expr::Object(name)
    }

    pub fn bool_const(value: bool) -> Expr {
        Expr::BoolConst(value)
    }
}




pub fn parse(input: &str) -> Result<Program, ParseError<usize, Token, LexicalError>> {
    let lexer = CoolLexer::new(input);
    let parser = ProgramTyParser::new();
    parser.parse(lexer)
}
