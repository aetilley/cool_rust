#![allow(clippy::self_named_constructors)]

use std::fmt::Debug;

use crate::cool_grammar::{FeatureTyParser, ProgramTyParser};
use crate::token::{CoolLexer, LexicalError, Token};

use lalrpop_util::ParseError;

pub trait Parse: Sized {
    fn parse(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    classes: Classes,
}
impl Program {
    pub fn program(classes: Classes) -> Program {
        Program { classes }
    }
}
impl Parse for Program {
    fn parse(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let lexer = CoolLexer::new(code);
        let parser = ProgramTyParser::new();
        parser.parse(lexer)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    name: String,
    features: Features,
}
pub type Classes = Vec<Class>;

impl Class {
    pub fn class(name: String, features: Features) -> Class {
        Class { name, features }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Formal {
    name: String,
    typ: String,
}
pub type Formals = Vec<Formal>;
impl Formal {
    pub fn formal(name: String, typ: String) -> Formal {
        Formal { name, typ }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Feature {
    Attr {
        name: String,
        typ: String,
        init: Expr,
    },
    Method {
        name: String,
        formals: Formals,
        typ: String,
        body: Expr,
    },
}
pub type Features = Vec<Feature>;

impl Feature {
    pub fn attr(name: String, typ: String, init: Expr) -> Feature {
        Feature::Attr { name, typ, init }
    }
    pub fn method(name: String, formals: Formals, typ: String, body: Expr) -> Feature {
        Feature::Method {
            name,
            formals,
            typ,
            body,
        }
    }
}
impl Parse for Feature {
    fn parse(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let lexer = CoolLexer::new(code);
        let parser = FeatureTyParser::new();
        parser.parse(lexer)
    }
}

#[derive(Clone, Debug, PartialEq)]
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


#[cfg(test)]
mod parse_tests {

    use super::*;

    #[test]
    fn test_simple_program() {
        let code: &str = r"
        class Apple {};
        class Orange {};
        ";
        let result = Program::parse(code).expect("Text program failed to parse");
        let desired_result = Program {
            classes: vec![
                Class {
                    name: "Apple".to_string(),
                    features: vec![],
                },
                Class {
                    name: "Orange".to_string(),
                    features: vec![],
                },
            ],
        };
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_attr1() {
        let code: &str = r"
            a: T1;
        ";
        let result = Feature::parse(code).expect("Text code failed to parse");
        let desired_result = Feature::Attr {
            name: "a".to_string(),
            typ: "T1".to_string(),
            init: Expr::NoExpr,
        };
        assert_eq!(result, desired_result);
    }
    #[test]
    fn test_attr2() {
        let code: &str = r"
            b: T3 <- true;
        ";
        let result = Feature::parse(code).expect("Text code failed to parse");
        let desired_result = Feature::Attr {
            name: "b".to_string(),
            typ: "T3".to_string(),
            init: Expr::BoolConst(true),
        };
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_method1() {
        let code: &str = r"
            foo() : T2 {true};
        ";
        let result = Feature::parse(code).expect("Text code failed to parse");
        let desired_result = Feature::Method {
            name: "foo".to_string(),
            formals: vec![],
            typ: "T2".to_string(),
            body: Expr::BoolConst(true),
        };
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_method2() {
        let code: &str = r"
            bar(c: S1, d: S2) : S3 {false};
        ";
        let result = Feature::parse(code).expect("Text program failed to parse");
        let desired_result = Feature::Method {
            name: "bar".to_string(),
            formals: vec![
                Formal {
                    name: "c".to_string(),
                    typ: "S1".to_string(),
                },
                Formal {
                    name: "d".to_string(),
                    typ: "S2".to_string(),
                },
            ],
            typ: "S3".to_string(),
            body: Expr::BoolConst(false),
        };
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_no_empty_method_body() {
        let code: &str = r"
            baz(a: S0) : T0 {};
        ";
        let result = Feature::parse(code);
        assert!(matches!(result, Err(_)));
    }
}
