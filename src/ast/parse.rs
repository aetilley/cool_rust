use std::collections::BTreeMap;

use crate::ast::cool_grammar::{ClassTyParser, ExprTyParser, FeatureTyParser, ProgramTyParser};
use crate::ast::token::{CoolLexer, LexicalError, Token};
use crate::ast::token_utils::{
    adjust_locations_in_parse_error, get_updated_span, strip_long_comments_and_get_insertion_map,
};
use crate::ast::{Case, Class, Expr, ExprData, Feature, Program};

use lalrpop_util::ParseError;

impl Program {
    pub fn parse_from(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        Parse::parse(code)
    }
}

pub trait Parse: Sized {
    fn inner_parser(lexer: CoolLexer) -> Result<Self, ParseError<usize, Token, LexicalError>>;

    fn parse(code: &str) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let (stripped, insertion_map) = strip_long_comments_and_get_insertion_map(code).unwrap();
        let lexer = CoolLexer::new(&stripped);
        match Self::inner_parser(lexer) {
            Ok(mut result) => {
                result.adjust_spans(&insertion_map);
                Ok(result)
            }
            Err(mut err) => {
                adjust_locations_in_parse_error(&mut err, &insertion_map);
                Err(err)
            }
        }
    }

    fn adjust_spans(&mut self, insertion_map: &BTreeMap<usize, usize>);
}

impl Parse for Program {
    fn inner_parser(lexer: CoolLexer) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let parser = ProgramTyParser::new();
        parser.parse(lexer)
    }

    fn adjust_spans(&mut self, insertion_map: &BTreeMap<usize, usize>) {
        for class in self.classes.iter_mut() {
            class.adjust_spans(insertion_map);
        }
    }
}

impl Parse for Class {
    fn inner_parser(lexer: CoolLexer) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let parser = ClassTyParser::new();
        parser.parse(lexer)
    }
    fn adjust_spans(&mut self, insertion_map: &BTreeMap<usize, usize>) {
        for feature in self.features.iter_mut() {
            feature.adjust_spans(insertion_map);
        }
    }
}

impl Parse for Feature {
    fn inner_parser(lexer: CoolLexer) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let parser = FeatureTyParser::new();
        parser.parse(lexer)
    }

    fn adjust_spans(&mut self, insertion_map: &BTreeMap<usize, usize>) {
        match self {
            Feature::Attr {
                name: _,
                typ: _,
                init,
            } => {
                init.adjust_spans(insertion_map);
            }
            Feature::Method {
                name: _,
                formals: _,
                typ: _,
                body,
            } => {
                body.adjust_spans(insertion_map);
            }
        }
    }
}

impl Case {
    fn adjust_spans(&mut self, insertion_map: &BTreeMap<usize, usize>) {
        self.expr.adjust_spans(insertion_map);
    }
}

impl Parse for Expr {
    fn inner_parser(lexer: CoolLexer) -> Result<Self, ParseError<usize, Token, LexicalError>> {
        let parser = ExprTyParser::new();
        parser.parse(lexer)
    }

    fn adjust_spans(&mut self, insertion_map: &BTreeMap<usize, usize>) {
        (*self.data).adjust_spans(insertion_map);
        self.span = get_updated_span(self.span, insertion_map);
    }
}

impl ExprData {
    fn adjust_spans(&mut self, insertion_map: &BTreeMap<usize, usize>) {
        match self {
            ExprData::Dispatch {
                slf,
                method_name: _,
                args,
            } => {
                slf.adjust_spans(insertion_map);
                args.iter_mut().for_each(|a| a.adjust_spans(insertion_map));
            }

            ExprData::StaticDispatch {
                typ: _,
                method_name: _,
                args,
                slf,
            } => {
                slf.adjust_spans(insertion_map);
                args.iter_mut().for_each(|a| a.adjust_spans(insertion_map));
            }

            ExprData::TypCase { expr, cases } => {
                expr.adjust_spans(insertion_map);
                cases.iter_mut().for_each(|a| a.adjust_spans(insertion_map));
            }

            ExprData::Let {
                id: _,
                typ: _,
                init,
                body,
            } => {
                init.adjust_spans(insertion_map);
                body.adjust_spans(insertion_map);
            }
            ExprData::Loop { pred, body } => {
                pred.adjust_spans(insertion_map);
                body.adjust_spans(insertion_map);
            }
            ExprData::Block { exprs } => {
                exprs.iter_mut().for_each(|a| a.adjust_spans(insertion_map));
            }
            ExprData::Cond {
                pred,
                then_expr,
                else_expr,
            } => {
                pred.adjust_spans(insertion_map);
                then_expr.adjust_spans(insertion_map);
                else_expr.adjust_spans(insertion_map);
            }
            ExprData::Assign { id: _, expr } => {
                expr.adjust_spans(insertion_map);
            }

            ExprData::Not { expr } | ExprData::IsVoid { expr } | ExprData::Comp { expr } => {
                expr.adjust_spans(insertion_map);
            }
            ExprData::Eq { lhs, rhs }
            | ExprData::Leq { lhs, rhs }
            | ExprData::Lt { lhs, rhs }
            | ExprData::Plus { lhs, rhs }
            | ExprData::Minus { lhs, rhs }
            | ExprData::Times { lhs, rhs }
            | ExprData::Divide { lhs, rhs } => {
                lhs.adjust_spans(insertion_map);
                rhs.adjust_spans(insertion_map);
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod parse_tests {

    use crate::ast::Formal;

    use super::*;

    #[test]
    fn test_simple_program() {
        let code: &str = r"
        class Apple {};
        class Orange inherits Bananas {};
        ";
        let result = Program::parse(code).expect("Test code failed to parse");
        let classes = vec![
            Class::class("Apple", "Object", vec![]),
            Class::class("Orange", "Bananas", vec![]),
        ];

        let desired_result = Program::program(classes);
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_attr1() {
        let code: &str = r"
            a: T1;
        ";
        let result = Feature::parse(code).expect("Test code failed to parse");
        let desired_result = Feature::attr("a", "T1", Expr::no_expr());
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_attr2() {
        let code: &str = r"
            b: T3 <- true;
        ";
        let result = Feature::parse(code).expect("Test code failed to parse");
        let desired_result = Feature::attr("b", "T3", Expr::bool_const(true));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_method1() {
        let code: &str = r"
            foo() : T2 {true};
        ";
        let result = Feature::parse(code).expect("Test code failed to parse");
        let desired_result = Feature::method("foo", vec![], "T2", Expr::bool_const(true));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_method2() {
        let code: &str = r"
            bar(c: S1, d: S2) : S3 {false};
        ";
        let result = Feature::parse(code).expect("Test code failed to parse");
        let desired_result = Feature::method(
            "bar",
            vec![Formal::formal("c", "S1"), Formal::formal("d", "S2")],
            "S3",
            Expr::bool_const(false),
        );
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
        let dcase1 = Case::case("a", "T1", Expr::object("expr1"));
        let dcase2 = Case::case("b", "T2", Expr::object("expr2"));
        let desired_result = Expr::typcase(Expr::object("bob"), vec![dcase1, dcase2]);
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_let() {
        let code: &str = r#"
            let bob: Int, carol: String <- "hello" in bob + y
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
                Expr::plus(Expr::object("bob"), Expr::object("y")),
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
    fn test_assign2() {
        let code: &str = r"
            a <- b <- c
        ";
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::assign("a", Expr::assign("b", Expr::object("c")));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_not_eq() {
        let code: &str = r"
            not a = b
        ";
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::not(Expr::eq(Expr::object("a"), Expr::object("b")));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_compare_and_arith() {
        let code1: &str = r"
            a < b + c
        ";
        let code2: &str = r"
            a + b < c
        ";
        let result1 = Expr::parse(code1).expect("Test code failed to parse");
        let result2 = Expr::parse(code2).expect("Test code failed to parse");
        let desired_result1 = Expr::lt(
            Expr::object("a"),
            Expr::plus(Expr::object("b"), Expr::object("c")),
        );
        let desired_result2 = Expr::lt(
            Expr::plus(Expr::object("a"), Expr::object("b")),
            Expr::object("c"),
        );
        assert_eq!(result1, desired_result1);
        assert_eq!(result2, desired_result2);
    }

    #[test]
    fn test_nested_arith() {
        let code: &str = r"
            a - b - c
        ";
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::minus(
            Expr::minus(Expr::object("a"), Expr::object("b")),
            Expr::object("c"),
        );
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_nested_arith_parents() {
        let code: &str = r"
            a - (b - c)
        ";
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::minus(
            Expr::object("a"),
            Expr::minus(Expr::object("b"), Expr::object("c")),
        );
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_compare_plus_times() {
        let code1: &str = r"
            a * b + c
        ";
        let code2: &str = r"
            a + b * c
        ";
        let result1 = Expr::parse(code1).expect("Test code failed to parse");
        let result2 = Expr::parse(code2).expect("Test code failed to parse");
        let desired_result1 = Expr::plus(
            Expr::times(Expr::object("a"), Expr::object("b")),
            Expr::object("c"),
        );
        let desired_result2 = Expr::plus(
            Expr::object("a"),
            Expr::times(Expr::object("b"), Expr::object("c")),
        );
        assert_eq!(result1, desired_result1);
        assert_eq!(result2, desired_result2);
    }

    #[test]
    fn test_comp() {
        let code: &str = r#"~x"#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::comp(Expr::object("x"));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_not_compare_comp() {
        let code: &str = r#"
           not x < ~ y 
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::not(Expr::lt(Expr::object("x"), Expr::comp(Expr::object("y"))));
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
        let desired_result = Expr::plus(
            Expr::object("x"),
            Expr::times(Expr::object("y"), Expr::object("z")),
        );
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_arith_in_parens() {
        let code: &str = r#"
            (x + y)
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::plus(Expr::object("x"), Expr::object("y"));
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_at_sign() {
        let code1: &str = r#"
            ~b@T.f()
        "#;
        let code2: &str = r#"
            a + b@T.f()
        "#;
        let result1 = Expr::parse(code1).expect("Test code failed to parse");
        let result2 = Expr::parse(code2).expect("Test code failed to parse");
        let desired_result1 =
            Expr::comp(Expr::static_dispatch(Expr::object("b"), "T", "f", vec![]));
        let desired_result2 = Expr::plus(
            Expr::object("a"),
            Expr::static_dispatch(Expr::object("b"), "T", "f", vec![]),
        );
        assert_eq!(result1, desired_result1);
        assert_eq!(result2, desired_result2);
    }

    #[test]
    fn test_arith_in_dispatch() {
        let code: &str = r#"
            g() - y
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::minus(
            Expr::dispatch(Expr::object("self"), "g", vec![]),
            Expr::object("y"),
        );
        assert_eq!(result, desired_result);
    }

    #[test]
    fn test_arith_in_if_1() {
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
    fn test_arith_in_if_2() {
        let code: &str = r#"
            if a then b else c fi + if d then e else f fi
        "#;
        let result = Expr::parse(code).expect("Test code failed to parse");
        let desired_result = Expr::plus(
            Expr::cond(Expr::object("a"), Expr::object("b"), Expr::object("c")),
            Expr::cond(Expr::object("d"), Expr::object("e"), Expr::object("f")),
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
