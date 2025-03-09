#![allow(clippy::self_named_constructors)]

use std::fmt::Debug;

use crate::cool_grammar::{ClassTyParser, ExprTyParser, FeatureTyParser, ProgramTyParser};
use crate::token::{strip_long_comments, CoolLexer, LexicalError, Token};

use lalrpop_util::ParseError;

pub fn add_one<T: Clone>(some: &Vec<T>, one: &T) -> Vec<T> {
    // Takes ownership
    let mut res = some.to_owned();
    res.push(one.to_owned());
    res
}

pub trait Parse: Sized {
    fn parse(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub classes: Classes,
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
    pub name: String,
    pub parent: String,
    pub features: Features,
}
pub type Classes = Vec<Class>;

impl Class {
    pub fn class(name: &str, parent: &str, features: Features) -> Class {
        Class {
            name: name.to_owned(),
            parent: parent.to_owned(),
            features,
        }
    }
}
impl Parse for Class {
    fn parse(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let stripped = strip_long_comments(code).unwrap();
        let lexer = CoolLexer::new(&stripped);
        let parser = ClassTyParser::new();
        parser.parse(lexer)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Formal {
    pub name: String,
    pub typ: String,
}
pub type Formals = Vec<Formal>;
impl Formal {
    pub fn formal(name: &str, typ: &str) -> Formal {
        Formal {
            name: name.to_owned(),
            typ: typ.to_owned(),
        }
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
    pub fn attr(name: &str, typ: &str, init: Expr) -> Feature {
        Feature::Attr {
            name: name.to_owned(),
            typ: typ.to_owned(),
            init,
        }
    }
    pub fn method(name: &str, formals: Formals, typ: &str, body: Expr) -> Feature {
        Feature::Method {
            name: name.to_owned(),
            formals,
            typ: typ.to_owned(),
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
    pub id: String,
    pub typ: String,
    pub expr: Expr,
}
pub type Cases = Vec<Case>;
impl Case {
    pub fn case(id: &str, typ: &str, expr: Expr) -> Self {
        Case {
            id: id.to_owned(),
            typ: typ.to_owned(),
            expr,
        }
    }
}

// Intermediate datastructure.  Will not end up in
// any final AST.
#[derive(Clone, Debug, PartialEq)]
pub struct LetBinding {
    id: String,
    typ: String,
    init: Expr,
}
pub type LetBindings = Vec<LetBinding>;
impl LetBinding {
    pub fn let_binding(id: &str, typ: &str, init: Expr) -> LetBinding {
        LetBinding {
            id: id.to_owned(),
            typ: typ.to_owned(),
            init,
        }
    }

    pub fn unwrap_let_bindings(let_bindings: LetBindings, body: Expr) -> Expr {
        if let_bindings.is_empty() {
            return body;
        }
        let head = let_bindings[0].to_owned();
        let tail = let_bindings[1..].to_owned();
        let new_body = LetBinding::unwrap_let_bindings(tail, body);
        Expr::r#let(&head.id, &head.typ, head.init, new_body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprData {
    NoExpr {},
    Object {
        id: String,
    },
    BoolConst {
        val: bool,
    },
    IntConst {
        val: String,
    },
    StrConst {
        val: String,
    },
    Dispatch {
        slf: Expr,
        method_name: String,
        args: Exprs,
    },
    StaticDispatch {
        typ: String,
        method_name: String,
        args: Exprs,
        slf: Expr,
    },
    New {
        typ: String,
    },
    TypCase {
        expr: Expr,
        cases: Cases,
    },
    Let {
        id: String,
        typ: String,
        init: Expr,
        body: Expr,
    },
    Loop {
        pred: Expr,
        body: Expr,
    },
    Block {
        exprs: Exprs,
    },
    Cond {
        pred: Expr,
        then_expr: Expr,
        else_expr: Expr,
    },
    Assign {
        id: String,
        expr: Expr,
    },
    Not {
        expr: Expr,
    },
    IsVoid {
        expr: Expr,
    },
    Comp {
        expr: Expr,
    },
    Eq {
        lhs: Expr,
        rhs: Expr,
    },
    Leq {
        lhs: Expr,
        rhs: Expr,
    },
    Lt {
        lhs: Expr,
        rhs: Expr,
    },
    Plus {
        lhs: Expr,
        rhs: Expr,
    },
    Minus {
        lhs: Expr,
        rhs: Expr,
    },
    Times {
        lhs: Expr,
        rhs: Expr,
    },
    Divide {
        lhs: Expr,
        rhs: Expr,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub data: Box<ExprData>,
    pub stype: String,
}
pub type Exprs = Vec<Expr>;
impl Expr {
    pub fn from(data: ExprData) -> Self {
        let stype = "No_type".to_owned();
        Expr {
            data: Box::new(data),
            stype,
        }
    }
}
impl Parse for Expr {
    fn parse(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let stripped = strip_long_comments(code).unwrap();
        let lexer = CoolLexer::new(&stripped);
        let parser = ExprTyParser::new();
        parser.parse(lexer)
    }
}

impl Expr {
    pub fn r#let(id: &str, typ: &str, init: Expr, body: Expr) -> Expr {
        Expr::from(ExprData::r#let(id, typ, init, body))
    }

    pub fn no_expr() -> Expr {
        Expr::from(ExprData::no_expr())
    }

    pub fn object(id: &str) -> Expr {
        Expr::from(ExprData::object(id))
    }

    pub fn bool_const(val: bool) -> Expr {
        Expr::from(ExprData::bool_const(val))
    }

    pub fn int_const(val: &str) -> Expr {
        Expr::from(ExprData::int_const(val))
    }

    pub fn str_const(val: &str) -> Expr {
        Expr::from(ExprData::str_const(val))
    }

    pub fn dispatch(slf: Expr, method_name: &str, args: Exprs) -> Expr {
        Expr::from(ExprData::dispatch(slf, method_name, args))
    }

    pub fn static_dispatch(slf: Expr, typ: &str, method_name: &str, args: Exprs) -> Expr {
        Expr::from(ExprData::static_dispatch(slf, typ, method_name, args))
    }

    pub fn new(typ: &str) -> Expr {
        Expr::from(ExprData::new(typ))
    }

    pub fn typcase(expr: Expr, cases: Cases) -> Expr {
        Expr::from(ExprData::typcase(expr, cases))
    }
    pub fn r#loop(pred: Expr, body: Expr) -> Expr {
        Expr::from(ExprData::r#loop(pred, body))
    }

    pub fn cond(pred: Expr, then_expr: Expr, else_expr: Expr) -> Expr {
        Expr::from(ExprData::cond(pred, then_expr, else_expr))
    }

    pub fn block(exprs: Exprs) -> Expr {
        Expr::from(ExprData::block(exprs))
    }

    pub fn assign(id: &str, expr: Expr) -> Expr {
        Expr::from(ExprData::assign(id, expr))
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(expr: Expr) -> Expr {
        Expr::from(ExprData::not(expr))
    }

    pub fn isvoid(expr: Expr) -> Expr {
        Expr::from(ExprData::isvoid(expr))
    }

    pub fn comp(expr: Expr) -> Expr {
        Expr::from(ExprData::comp(expr))
    }

    pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::lt(lhs, rhs))
    }

    pub fn leq(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::leq(lhs, rhs))
    }

    pub fn eq(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::eq(lhs, rhs))
    }

    pub fn plus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::plus(lhs, rhs))
    }

    pub fn minus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::minus(lhs, rhs))
    }

    pub fn times(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::times(lhs, rhs))
    }

    pub fn divide(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::divide(lhs, rhs))
    }
}

impl ExprData {
    pub fn r#let(id: &str, typ: &str, init: Expr, body: Expr) -> ExprData {
        ExprData::Let {
            id: id.to_owned(),
            typ: typ.to_owned(),
            init,
            body,
        }
    }

    pub fn no_expr() -> ExprData {
        ExprData::NoExpr {}
    }

    pub fn object(id: &str) -> ExprData {
        ExprData::Object { id: id.to_owned() }
    }

    pub fn bool_const(val: bool) -> ExprData {
        ExprData::BoolConst { val }
    }

    pub fn int_const(val: &str) -> ExprData {
        ExprData::IntConst {
            val: val.to_owned(),
        }
    }

    pub fn str_const(val: &str) -> ExprData {
        ExprData::StrConst {
            val: val.to_owned(),
        }
    }

    pub fn dispatch(slf: Expr, method_name: &str, args: Exprs) -> ExprData {
        ExprData::Dispatch {
            slf,
            method_name: method_name.to_owned(),
            args,
        }
    }

    pub fn static_dispatch(elem: Expr, typ: &str, method_name: &str, args: Exprs) -> ExprData {
        ExprData::StaticDispatch {
            slf: elem,
            typ: typ.to_owned(),
            method_name: method_name.to_owned(),
            args,
        }
    }

    pub fn new(typ: &str) -> ExprData {
        ExprData::New {
            typ: typ.to_owned(),
        }
    }

    pub fn typcase(expr: Expr, cases: Cases) -> ExprData {
        ExprData::TypCase { expr, cases }
    }
    pub fn r#loop(pred: Expr, body: Expr) -> ExprData {
        ExprData::Loop { pred, body }
    }

    pub fn cond(pred: Expr, then_expr: Expr, else_expr: Expr) -> ExprData {
        ExprData::Cond {
            pred,
            then_expr,
            else_expr,
        }
    }

    pub fn block(exprs: Exprs) -> ExprData {
        ExprData::Block { exprs }
    }

    pub fn assign(id: &str, expr: Expr) -> ExprData {
        ExprData::Assign {
            id: id.to_owned(),
            expr,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(expr: Expr) -> ExprData {
        ExprData::Not { expr }
    }

    pub fn isvoid(expr: Expr) -> ExprData {
        ExprData::IsVoid { expr }
    }

    pub fn comp(expr: Expr) -> ExprData {
        ExprData::Comp { expr }
    }

    pub fn lt(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Lt { lhs, rhs }
    }

    pub fn leq(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Leq { lhs, rhs }
    }

    pub fn eq(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Eq { lhs, rhs }
    }

    pub fn plus(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Plus { lhs, rhs }
    }

    pub fn minus(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Minus { lhs, rhs }
    }

    pub fn times(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Times { lhs, rhs }
    }

    pub fn divide(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Divide { lhs, rhs }
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
        let result = Program::parse(code).expect("Test code failed to parse");
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
        let result = Feature::parse(code).expect("Test code failed to parse");
        let desired_result = Feature::Attr {
            name: "a".to_string(),
            typ: "T1".to_string(),
            init: Expr::no_expr(),
        };
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_attr2() {
        let code: &str = r"
            b: T3 <- true;
        ";
        let result = Feature::parse(code).expect("Test code failed to parse");
        let desired_result = Feature::Attr {
            name: "b".to_string(),
            typ: "T3".to_string(),
            init: Expr::bool_const(true),
        };
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_method1() {
        let code: &str = r"
            foo() : T2 {true};
        ";
        let result = Feature::parse(code).expect("Test code failed to parse");
        let desired_result = Feature::Method {
            name: "foo".to_string(),
            formals: vec![],
            typ: "T2".to_string(),
            body: Expr::bool_const(true),
        };
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_method2() {
        let code: &str = r"
            bar(c: S1, d: S2) : S3 {false};
        ";
        let result = Feature::parse(code).expect("Test code failed to parse");
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
            body: Expr::bool_const(false),
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

    #[test]
    fn test_noexpr() {
        let code: &str = r"
        karen
        ";
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::object("karen");
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_boolconst() {
        let code1: &str = r"
        true
        ";
        let code2: &str = r"
        false
        ";
        let result1 = Expr::parse(code1).expect("Test code failed to parse");
        let result2 = Expr::parse(code2).expect("Test code failed to parse");
        let desired_result1 = Expr::bool_const(true);
        let desired_result2 = Expr::bool_const(false);
        assert_eq!(result1, desired_result1);
        assert_eq!(result2, desired_result2);
    }

    #[test]
    fn test_intconst() {
        let code1: &str = r"
        42
        ";
        let code2: &str = r"
        0
        ";
        let result1 = Expr::parse(code1).expect("Test code failed to parse");
        let result2 = Expr::parse(code2).expect("Test code failed to parse");
        let desired_result1 = Expr::int_const("42");
        let desired_result2 = Expr::int_const("0");
        assert_eq!(result1, desired_result1);
        assert_eq!(result2, desired_result2);
    }

    #[test]
    fn test_strconst() {
        let code: &str = r#"
        "abc"
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::str_const("abc");
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_dispatch() {
        let code: &str = r#"
        a.foo("hello", 42)
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::dispatch(
            Expr::object("a"),
            "foo",
            vec![Expr::str_const("hello"), Expr::int_const("42")],
        );
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_static_dispatch() {
        // TODO!
        let code: &str = r#"
        a@Apples.foo("hello", 42)
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::static_dispatch(
            Expr::object("a"),
            "Apples",
            "foo",
            vec![Expr::str_const("hello"), Expr::int_const("42")],
        );
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_new() {
        let code: &str = r"
            new T
        ";
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::new("T");
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_typcase() {
        let code: &str = r"
            case bob of
                a: T1 => expr1;
                b: T2 => expr2;
            esac
        ";
        let result = Expr::parse(code).expect("Test code failed to parse");
        let dcase1 = Case {
            id: "a".to_owned(),
            typ: "T1".to_owned(),
            expr: Expr::object("expr1"),
        };
        let dcase2 = Case {
            id: "b".to_owned(),
            typ: "T2".to_owned(),
            expr: Expr::object("expr2"),
        };
        let desired_result = Expr::typcase(Expr::object("bob"), vec![dcase1, dcase2]);
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_let() {
        let code: &str = r#"
            let bob: Int, carol: String <- "hello" in somebody
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::r#let(
            "bob",
            "Int",
            Expr::no_expr(),
            Expr::r#let(
                "carol",
                "String",
                Expr::str_const("hello"),
                Expr::object("somebody"),
            ),
        );
        assert_eq!(result, desired_result);
    }
    #[test]
    fn test_loop() {
        let code: &str = r#"
            while somepred loop somebody pool 
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::r#loop(Expr::object("somepred"), Expr::object("somebody"));
        assert_eq!(result, desired_result);
    }
    #[test]
    fn test_block() {
        let code: &str = r#"
            { e1; e2; e3;}
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::block(vec![
            Expr::object("e1"),
            Expr::object("e2"),
            Expr::object("e3"),
        ]);
        assert_eq!(result, desired_result);
    }
    #[test]
    fn test_cond() {
        let code: &str = r#"
            if somepred then somethen else someelse fi
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::cond(
            Expr::object("somepred"),
            Expr::object("somethen"),
            Expr::object("someelse"),
        );
        assert_eq!(result, desired_result);
    }
    #[test]
    fn test_assign() {
        let code: &str = r#"
           a  <- b
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::assign("a", Expr::object("b"));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_not() {
        let code: &str = r#"
           not x
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::not(Expr::object("x"));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_isvoid() {
        let code: &str = r#"
           isvoid x
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::isvoid(Expr::object("x"));
        assert_eq!(result, desired_result);
    }
    #[test]
    fn test_comp() {
        let code: &str = r#"~x"#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::comp(Expr::object("x"));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_comparisons() {
        let code_eq: &str = r#"
           x = y
        "#;
        let code_leq: &str = r#"
           x <= y
        "#;
        let code_lt: &str = r#"
           x < y
        "#;
        let result_eq = Expr::parse(code_eq).expect("Test code failed to parse");
        let result_leq = Expr::parse(code_leq).expect("Test code failed to parse");
        let result_lt = Expr::parse(code_lt).expect("Test code failed to parse");
        let desired_result_eq = Expr::eq(Expr::object("x"), Expr::object("y"));
        let desired_result_leq = Expr::leq(Expr::object("x"), Expr::object("y"));
        let desired_result_lt = Expr::lt(Expr::object("x"), Expr::object("y"));
        assert_eq!(result_eq, desired_result_eq);
        assert_eq!(result_leq, desired_result_leq);
        assert_eq!(result_lt, desired_result_lt);
    }

    #[test]
    fn test_precedence_1() {
        let code: &str = r#"
            x + y * z
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        println!("{:?}", result);
    }

    // #[test]
    fn test_arith_in_parens() {
        let code: &str = r#"
            (x + y)
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::plus(Expr::object("x"), Expr::object("y"));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_arith_in_dispatch() {
        let code: &str = r#"
            g() - y
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::minus(
            Expr::dispatch(Expr::no_expr(), "g", vec![]),
            Expr::object("y"),
        );
        assert_eq!(result, desired_result);
    }

    //#[test]
    fn test_arith_in_if() {
        let code: &str = r#"
            if x + y then a else b fi
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::cond(
            Expr::plus(Expr::object("x"), Expr::object("y")),
            Expr::object("a"),
            Expr::object("b"),
        );
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_arithmetic() {
        let code_plus: &str = r#"
           x + y
        "#;
        let code_minus: &str = r#"
           x - y
        "#;
        let code_times: &str = r#"
           x * y
        "#;
        let code_divide: &str = r#"
           x / y
        "#;
        let result_plus = Expr::parse(code_plus).expect("Test code failed to parse");
        let result_minus = Expr::parse(code_minus).expect("Test code failed to parse");
        let result_times = Expr::parse(code_times).expect("Test code failed to parse");
        let result_divide = Expr::parse(code_divide).expect("Test code failed to parse");
        let desired_result_plus = Expr::plus(Expr::object("x"), Expr::object("y"));
        let desired_result_minus = Expr::minus(Expr::object("x"), Expr::object("y"));
        let desired_result_times = Expr::times(Expr::object("x"), Expr::object("y"));
        let desired_result_divide = Expr::divide(Expr::object("x"), Expr::object("y"));
        assert_eq!(result_plus, desired_result_plus);
        assert_eq!(result_minus, desired_result_minus);
        assert_eq!(result_times, desired_result_times);
        assert_eq!(result_divide, desired_result_divide);
    }
}
