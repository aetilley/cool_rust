#![allow(clippy::self_named_constructors)]

use std::fmt::Debug;

use crate::cool_grammar::{FeatureTyParser, ProgramTyParser};
use crate::token::{strip_long_comments, CoolLexer, LexicalError, Token};

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
        let stripped = strip_long_comments(code).unwrap();
        let lexer = CoolLexer::new(&stripped);
        let parser = ProgramTyParser::new();
        parser.parse(lexer)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    name: String,
    parent: String,
    features: Features,
}
pub type Classes = Vec<Class>;

impl Class {
    pub fn class(name: String, parent: String, features: Features) -> Class {
        Class {
            name,
            parent,
            features,
        }
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
        let stripped = strip_long_comments(code).unwrap();
        let lexer = CoolLexer::new(&stripped);
        let parser = FeatureTyParser::new();
        parser.parse(lexer)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    id: String,
    typ: String,
    expr: Expr,
}
pub type Cases = Vec<Case>;
impl Case {
    pub fn case(id: String, typ: String, expr: Expr) -> Self {
        Case { id, typ, expr }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    NoExpr,
    Object(String),
    BoolConst(bool),
    IntConst(String),
    StrConst(String),
    Dispatch(Box<Expr>, String, Exprs),
    StaticDispatch(Box<Expr>, String, String, Exprs),
    New(String),
    TypCase(Box<Expr>, Cases),
    Loop(Box<Expr>, Box<Expr>),
    Block(Exprs),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Assign(String, Box<Expr>),
    Not(Box<Expr>),
    IsVoid(Box<Expr>),
    Comp(Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Leq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
}
pub type Exprs = Vec<Expr>;

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

    pub fn int_const(value: String) -> Expr {
        Expr::IntConst(value)
    }

    pub fn str_const(value: String) -> Expr {
        Expr::StrConst(value)
    }

    pub fn dispatch(elem: Expr, meth_name: String, args: Exprs) -> Expr {
        Expr::Dispatch(Box::<Expr>::new(elem), meth_name, args)
    }

    pub fn static_dispatch(elem: Expr, typ: String, meth_name: String, args: Exprs) -> Expr {
        Expr::StaticDispatch(Box::<Expr>::new(elem), typ, meth_name, args)
    }

    pub fn new(typ: String) -> Expr {
        Expr::New(typ)
    }

    pub fn typcase(expr: Expr, cases: Cases) -> Expr {
        Expr::TypCase(Box::<Expr>::new(expr), cases)
    }
    pub fn r#loop(cond: Expr, body: Expr) -> Expr {
        Expr::Loop(Box::<Expr>::new(cond), Box::<Expr>::new(body))
    }

    pub fn cond(pred: Expr, then_expr: Expr, else_expr: Expr) -> Expr {
        Expr::Cond(
            Box::<Expr>::new(pred),
            Box::<Expr>::new(then_expr),
            Box::<Expr>::new(else_expr),
        )
    }

    pub fn block(body: Exprs) -> Expr {
        Expr::Block(body)
    }

    pub fn assign(name: String, expr: Expr) -> Expr {
        Expr::Assign(name, Box::<Expr>::new(expr))
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(expr: Expr) -> Expr {
        Expr::Not(Box::<Expr>::new(expr))
    }

    pub fn isvoid(expr: Expr) -> Expr {
        Expr::IsVoid(Box::<Expr>::new(expr))
    }

    pub fn comp(expr: Expr) -> Expr {
        Expr::IsVoid(Box::<Expr>::new(expr))
    }

    pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Lt(Box::<Expr>::new(lhs), Box::<Expr>::new(rhs))
    }

    pub fn leq(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Leq(Box::<Expr>::new(lhs), Box::<Expr>::new(rhs))
    }

    pub fn eq(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Eq(Box::<Expr>::new(lhs), Box::<Expr>::new(rhs))
    }

    pub fn plus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Plus(Box::<Expr>::new(lhs), Box::<Expr>::new(rhs))
    }

    pub fn minus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Minus(Box::<Expr>::new(lhs), Box::<Expr>::new(rhs))
    }

    pub fn times(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Times(Box::<Expr>::new(lhs), Box::<Expr>::new(rhs))
    }

    pub fn divide(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Divide(Box::<Expr>::new(lhs), Box::<Expr>::new(rhs))
    }
}

#[cfg(test)]
mod parse_tests {

    use super::*;

    #[test]
    fn test_simple_program() {
        let code: &str = r"
        class Apple {};
        class Orange inherits Bananas {};
        ";
        let result = Program::parse(code).expect("Text program failed to parse");
        let desired_result = Program {
            classes: vec![
                Class {
                    name: "Apple".to_string(),
                    parent: "Object".to_string(),
                    features: vec![],
                },
                Class {
                    name: "Orange".to_string(),
                    parent: "Bananas".to_string(),
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
        assert!(result.is_err());
    }
}
