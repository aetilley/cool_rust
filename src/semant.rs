/*
This module provides functionality to
1) Do scope- and type-checking of Program.
2) Populate the type field of an Expr.
*/

#[allow(unused_imports)]
use crate::ast::{
    Case, Cases, Class, Classes, Expr, ExprData, Exprs, Feature, Features, Formal, Formals,
    LetBinding, LetBindings, Program,
};
use crate::class_table::ClassTable;
use crate::env::Env;

#[derive(Debug, PartialEq)]
pub struct SemanticAnalysisError {
    pub msg: String,
}

impl Program {
    pub fn semant(&mut self) -> Result<(), SemanticAnalysisError> {
        // Entry point for semantic analysis.  Takes a mutable ref to self
        // because it will populate the type field on descendant asts.

        // Analyze the class inheritance graph of a Program to verify that
        // there are no cycles.
        let ct = ClassTable::new(&self.classes)?;

        let mut env = Env::new();

        for class in self.classes.iter_mut() {
            class.semant(&ct, &mut env)?;
        }

        Ok(())
    }
}

impl Class {
    pub fn semant(&mut self, ct: &ClassTable, env: &mut Env) -> Result<(), SemanticAnalysisError> {
        env.enter_scope();

        env.add_binding("self", self.name.as_ref());

        for feature in self.features.iter_mut() {
            feature.analyze(ct, env, &self.name.to_owned())?;
        }

        env.exit_scope();
        Ok(())
    }
}

pub trait Analyze {
    fn analyze(
        &mut self,
        ct: &ClassTable,
        env: &mut Env,
        cls: &str,
    ) -> Result<(), SemanticAnalysisError>;
}

impl Analyze for Feature {
    fn analyze(
        &mut self,
        ct: &ClassTable,
        env: &mut Env,
        cls: &str,
    ) -> Result<(), SemanticAnalysisError> {
        match self {
            Feature::Attr { name, typ, init } => {
                init.analyze(ct, env, cls)?;

                if init.stype != "No_type" && ct.assert_subtype(&init.stype, typ).is_err() {
                    let msg = format!(
                        "In class {}, attribute {} type declared to be {}, but found {}",
                        cls, name, typ, init.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                env.add_binding(name.as_ref(), typ.as_ref());
            }
            Feature::Method {
                name,
                formals,
                typ,
                body,
            } => {
                env.enter_scope();
                for formal in formals {
                    env.add_binding(formal.name.as_ref(), formal.typ.as_ref());
                }
                body.analyze(ct, env, cls)?;
                env.exit_scope();

                if ct.assert_subtype(&body.stype, typ).is_err() {
                    let msg = format!(
                        "In class {}, method {} type declared to be {}, but found {}",
                        cls, name, typ, body.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
            }
        }
        Ok(())
    }
}

impl Analyze for Case {
    fn analyze(
        &mut self,
        ct: &ClassTable,
        env: &mut Env,
        cls: &str,
    ) -> Result<(), SemanticAnalysisError> {
        env.enter_scope();
        env.add_binding(&self.id, &self.typ);
        self.expr.analyze(ct, env, cls)?;
        env.exit_scope();
        Ok(())
    }
}

impl Analyze for Expr {
    fn analyze(
        &mut self,
        ct: &ClassTable,
        env: &mut Env,
        cls: &str,
    ) -> Result<(), SemanticAnalysisError> {
        // Set the `self.stype` field.
        let stype: String;

        match &mut *self.data {
            ExprData::Dispatch {
                slf,
                method_name,
                args,
            } => {
                slf.analyze(ct, env, cls)?;

                let param_types = ct.get_param_types(&slf.stype, method_name)?;

                for (next_param, arg) in args.iter_mut().enumerate() {
                    arg.analyze(ct, env, cls)?;
                    if ct
                        .assert_subtype(&arg.stype, &param_types[next_param].typ)
                        .is_err()
                    {
                        let msg = format!(
                            "In class {}, method {} argument {} type declared to be {}, but found {}",
                            cls, method_name, param_types[next_param].name, param_types[next_param].typ, arg.stype
                        );
                        return Err(SemanticAnalysisError { msg });
                    }
                }

                let mut return_type = ct.get_return_type(&slf.stype, method_name)?;

                if return_type == "SELF_TYPE" {
                    return_type = slf.stype.clone();
                }
                stype = return_type;
            }
            ExprData::StaticDispatch {
                typ,
                method_name,
                args,
                slf,
            } => {
                slf.analyze(ct, env, cls)?;
                if ct.assert_subtype(&slf.stype, typ).is_err() {
                    let msg = format!(
                        "In class {}, method {} dispatch type declared to be {}, 
                        but found self expression of non-subtype type {}",
                        cls, method_name, typ, slf.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }

                let param_types = ct.get_param_types(typ, method_name)?;

                for (next_param, arg) in args.iter_mut().enumerate() {
                    arg.analyze(ct, env, cls)?;
                    if ct
                        .assert_subtype(&arg.stype, &param_types[next_param].typ)
                        .is_err()
                    {
                        let msg = format!(
                            "In class {}, method {} argument {} type declared to be {}, 
                            but found {}",
                            cls,
                            method_name,
                            param_types[next_param].name,
                            param_types[next_param].typ,
                            arg.stype
                        );
                        return Err(SemanticAnalysisError { msg });
                    }
                }

                let mut return_type = ct.get_return_type(typ, method_name)?;

                if return_type == "SELF_TYPE" {
                    return_type = slf.stype.clone();
                }
                stype = return_type;
            }

            ExprData::TypCase { expr, cases } => {
                let mut lub = "No_type".to_owned();
                expr.analyze(ct, env, cls)?;
                for case in cases.iter_mut() {
                    case.analyze(ct, env, cls)?;
                    lub = ct.get_lub(&lub, &case.expr.stype);
                }
                stype = lub;
            }

            ExprData::Let {
                id,
                typ,
                init,
                body,
            } => {
                init.analyze(ct, env, cls)?;
                if ct.assert_subtype(&init.stype, typ).is_err() {
                    let msg = format!(
                        "In class {}, let variable declared to be {}, but found initializer of non-subtype type {}",
                        cls, typ, init.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }

                env.enter_scope();

                env.add_binding(id, typ);

                body.analyze(ct, env, cls)?;

                env.exit_scope();

                stype = body.stype.clone();
            }

            ExprData::Loop { pred, body } => {
                pred.analyze(ct, env, cls)?;
                if &pred.stype != "Bool" {
                    let msg = format!(
                        "In class {}, while-loop predicate was of non-Bool type {}",
                        cls, pred.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                body.analyze(ct, env, cls)?;
                stype = "Object".to_owned();
            }

            ExprData::Cond {
                pred,
                then_expr,
                else_expr,
            } => {
                pred.analyze(ct, env, cls)?;
                if &pred.stype != "Bool" {
                    let msg = format!(
                        "In class {}, conditional predicate was of non-Bool type {}",
                        cls, pred.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                then_expr.analyze(ct, env, cls)?;
                else_expr.analyze(ct, env, cls)?;
                stype = ct.get_lub(&then_expr.stype, &else_expr.stype);
            }

            ExprData::Block { exprs } => {
                for expr in exprs.iter_mut() {
                    expr.analyze(ct, env, cls)?;
                }
                match exprs.last() {
                    Some(expr) => {
                        stype = expr.stype.clone();
                    }
                    None => {
                        panic!(
                            "Empty block expression.  This should have been caught by the parser."
                        )
                    }
                }
            }

            ExprData::Assign { id, expr } => {
                expr.analyze(ct, env, cls)?;
                match env.lookup(id) {
                    None => {
                        let msg = format!("No object named \"{}\" found in scope.", id);
                        return Err(SemanticAnalysisError { msg });
                    }
                    Some(var_type) => {
                        ct.assert_subtype(&expr.stype, &var_type)?;
                        if ct.assert_subtype(&expr.stype, &var_type).is_err() {
                            let msg = format!(
                                "In class {}, assignment variable previously declared to be of type {}, but assigned to non-subtype {}",
                                cls, var_type, expr.stype
                            );
                            return Err(SemanticAnalysisError { msg });
                        }
                        stype = var_type;
                    }
                }
            }
            ExprData::NoExpr {} => {
                stype = "No_type".to_owned();
            }
            ExprData::IntConst { val: _ } => {
                stype = "Int".to_owned();
            }
            ExprData::StrConst { val: _ } => {
                stype = "Str".to_owned();
            }
            ExprData::BoolConst { val: _ } => {
                stype = "Bool".to_owned();
            }
            ExprData::IsVoid { expr } => {
                expr.analyze(ct, env, cls)?;
                if &expr.stype != "Bool" {
                    let msg = format!(
                        "In class {}, isvoid arg was of non-Bool type {}",
                        cls, expr.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = "Bool".to_owned();
            }
            ExprData::New { typ } => {
                stype = typ.to_owned();
            }
            ExprData::Not { expr } => {
                expr.analyze(ct, env, cls)?;
                if &expr.stype != "Bool" {
                    let msg = format!(
                        "In class {}, not arg was of non-Bool type {}",
                        cls, expr.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = "Bool".to_owned();
            }
            ExprData::Comp { expr } => {
                expr.analyze(ct, env, cls)?;
                if &expr.stype != "Int" {
                    let msg = format!(
                        "In class {}, comp arg was of non-Int type {}",
                        cls, expr.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = "Int".to_owned();
            }
            ExprData::Lt { lhs, rhs } | ExprData::Leq { lhs, rhs } => {
                lhs.analyze(ct, env, cls)?;
                if &lhs.stype != "Int" {
                    let msg = format!(
                        "In class {}, operator lhs was of non-Int type {}",
                        cls, lhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                rhs.analyze(ct, env, cls)?;
                if &rhs.stype != "Int" {
                    let msg = format!(
                        "In class {}, operator rhs was of non-Int type {}",
                        cls, rhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = "Bool".to_owned();
            }
            ExprData::Eq { lhs, rhs } => {
                // Unlike < and <=, the = operator can take arbitrary types.
                lhs.analyze(ct, env, cls)?;
                rhs.analyze(ct, env, cls)?;

                let mut comp_as_type: &str = "Object";
                if (&lhs.stype == "Int") | (&lhs.stype == "Str") | (&lhs.stype == "Bool") {
                    comp_as_type = &lhs.stype;
                }
                if (&rhs.stype == "Int") | (&rhs.stype == "Str") | (&rhs.stype == "Bool") {
                    comp_as_type = &rhs.stype;
                }
                if comp_as_type != "Object" && lhs.stype != rhs.stype {
                    let msg = format!(
                        "In class {}, equality operands had incompatible types lhs: {} and rhs: {}.",
                        cls, lhs.stype, rhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }

                stype = "Bool".to_owned();
            }

            ExprData::Plus { lhs, rhs }
            | ExprData::Minus { lhs, rhs }
            | ExprData::Times { lhs, rhs }
            | ExprData::Divide { lhs, rhs } => {
                lhs.analyze(ct, env, cls)?;
                if &lhs.stype != "Int" {
                    let msg = format!(
                        "In class {}, operator lhs was of non-Int type {}",
                        cls, lhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                rhs.analyze(ct, env, cls)?;
                if &rhs.stype != "Int" {
                    let msg = format!(
                        "In class {}, operator rhs was of non-Int type {}",
                        cls, rhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = "Int".to_owned();
            }
            ExprData::Object { id } => match env.lookup(id) {
                None => {
                    let msg = format!("No object named \"{}\" found in scope.", id);
                    return Err(SemanticAnalysisError { msg });
                }
                Some(value) => stype = value,
            },
        }
        self.stype = stype;
        Ok(())
    }
}

#[cfg(test)]
mod semant_tests {

    use super::*;
    use crate::ast::Parse;

    #[test]
    fn test_semant_simple_program() {
        let code: &str = r"
        class Apple {};
        class Orange inherits Bananas {};
        class Bananas {};
        ";

        let mut program = Program::parse(code).expect("Test code failed to parse");
        let result = program.semant();
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn test_semant_features() {
        let c1: &str = r"
        a: Int <- 42;
        ";
        let c2: &str = r"
        b: Banana;
        ";
        let c3: &str = r"
        c: Banana <- b;
        ";
        let c4: &str = r"
        foo(): Banana {c};
        ";

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let mut f1 = Feature::parse(c1).unwrap();
        let result = f1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut f2 = Feature::parse(c2).unwrap();
        let result = f2.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut f3 = Feature::parse(c3).unwrap();
        let result = f3.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut f4 = Feature::parse(c4).unwrap();
        let result = f4.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut desired_env = Env::new();
        desired_env.add_binding("a", "Int");
        desired_env.add_binding("b", "Banana");
        desired_env.add_binding("c", "Banana");
        assert_eq!(env, desired_env);
    }

    #[test]
    fn test_semant_dispatch() {
        let cls_cd: &str = r"
        class Apple {foo(): Banana {42};};
        ";
        let attr_cd: &str = r"
        a: Apple;
        ";
        let expr_cd: &str = r"
        a.foo()
        ";

        let cls = Class::parse(cls_cd).unwrap();
        let ct = ClassTable::new(&vec![cls]).unwrap();
        let mut env = Env::new();

        let mut attr = Feature::parse(attr_cd).unwrap();
        let result = attr.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, "Banana");
    }

    #[test]
    fn test_semant_static_dispatch() {
        let cls_cd_1: &str = r"
        class Apple {foo(): Banana {42};};
        ";

        let cls_cd_2: &str = r"
        class Kiwi inherits Apple {};
        ";

        let attr_cd: &str = r"
        a: Kiwi;
        ";
        let expr_cd: &str = r"
        a@Apple.foo()
        ";

        let cls_1 = Class::parse(cls_cd_1).unwrap();
        let cls_2 = Class::parse(cls_cd_2).unwrap();
        let ct = ClassTable::new(&vec![cls_1, cls_2]).unwrap();
        let mut env = Env::new();

        let mut attr = Feature::parse(attr_cd).unwrap();
        let result = attr.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, "Banana");
    }

    // #[test]
    fn test_semant_dynamic_dispatch_nontrivial() {
        let cls_cd_1: &str = r"
        class Apple {foo(): Banana {42};};
        ";

        let cls_cd_2: &str = r"
        class Kiwi inherits Apple {};
        ";

        let attr_cd: &str = r"
        a: Kiwi;
        ";
        let expr_cd: &str = r"
        a.foo()
        ";

        let cls_1 = Class::parse(cls_cd_1).unwrap();
        let cls_2 = Class::parse(cls_cd_2).unwrap();
        let ct = ClassTable::new(&vec![cls_1, cls_2]).unwrap();
        let mut env = Env::new();

        let mut attr = Feature::parse(attr_cd).unwrap();
        let result = attr.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, "Banana");
    }

    #[test]
    fn test_semant_exprs_int() {
        let c1: &str = r"
        42
        ";
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, "Int".to_string());
    }

    #[test]
    fn test_semant_exprs_str() {
        let c1: &str = r#"
        "42"
        "#;
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, "Str".to_string());
    }
    #[test]
    fn test_semant_exprs_bool() {
        let c1: &str = "
        true
        ";
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, "Bool".to_string());
    }

    #[test]
    fn test_semant_exprs_plus() {
        let c1: &str = r"
        42 + 2
        ";
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, "Int".to_string());
    }

    #[test]
    fn test_semant_simple_lookup() {
        let c1: &str = r"
        a: Apple;
        ";
        let c2: &str = r"
        b: Banana;
        ";

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let mut f1 = Feature::parse(c1).unwrap();
        let result = f1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut f2 = Feature::parse(c2).unwrap();
        let result = f2.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let c1: &str = r#"
        a
        "#;
        let c2: &str = r#"
        b
        "#;
        let mut e1 = Expr::parse(c1).unwrap();
        let mut e2 = Expr::parse(c2).unwrap();
        let result1 = e1.analyze(&ct, &mut env, "UNUSED");
        let result2 = e2.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result1, Ok(()));
        assert_eq!(result2, Ok(()));
        assert_eq!(e1.stype, "Apple");
        assert_eq!(e2.stype, "Banana");
    }

    #[test]
    fn test_semant_cond() {
        let c1: &str = r"
        a: Int;
        ";
        let c2: &str = r"
        b: Object;
        ";
        let c3: &str = r"
        c: Bool;
        ";
        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let mut f1 = Feature::parse(c1).unwrap();
        let result = f1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut f2 = Feature::parse(c2).unwrap();
        let result = f2.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let mut f3 = Feature::parse(c3).unwrap();
        let result = f3.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));

        let code: &str = r#"
        if c then a else b fi
        "#;
        let mut expr = Expr::parse(code).expect("Test code failed to parse");
        let result = expr.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        assert_eq!(expr.stype, "Object");

        let code: &str = r#"
        if a then a else b fi
        "#;
        let mut expr = Expr::parse(code).expect("Test code failed to parse");
        let result = expr.analyze(&ct, &mut env, "UNUSED");
        assert!(result.is_err());
    }

    #[test]
    fn test_semant_exprs_block() {
        let c: &str = r#"
        {
        "hello";
        42;
        }
        "#;
        let mut e1 = Expr::parse(c).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, "Int".to_string());
    }
}
