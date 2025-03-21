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
use crate::symbol::{sym, Sym};

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

        env.add_binding(&sym("self"), &sym("SELF_TYPE"));

        for feature in self.features.iter_mut() {
            feature.analyze(ct, env, &self.name)?;
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
        cls: &Sym,
    ) -> Result<(), SemanticAnalysisError>;
}

impl Analyze for Feature {
    fn analyze(
        &mut self,
        ct: &ClassTable,
        env: &mut Env,
        cls: &Sym,
    ) -> Result<(), SemanticAnalysisError> {
        match self {
            Feature::Attr { name, typ, init } => {
                init.analyze(ct, env, cls)?;

                if init.stype != "No_type" && ct.assert_subtype(&init.stype, typ, cls).is_err() {
                    let msg = format!(
                        "Attribute {} type declared to be {}, \
                        but found type {} for initializer at {:?} ",
                        name, typ, init.stype, init.span
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                env.add_binding(name, typ);
            }
            Feature::Method {
                name,
                formals,
                typ,
                body,
            } => {
                env.enter_scope();
                for formal in formals {
                    env.add_binding(&formal.name, &formal.typ);
                }
                body.analyze(ct, env, cls)?;
                env.exit_scope();

                if ct.assert_subtype(&body.stype, typ, cls).is_err() {
                    let msg = format!(
                        "Method {} type declared to be {}, but found type {} for body at {:?}",
                        name, typ, body.stype, body.span
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
        cls: &Sym,
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
        cls: &Sym,
    ) -> Result<(), SemanticAnalysisError> {
        // Set the `self.stype` field.
        let span = self.span;

        let stype: Sym;

        match &mut *self.data {
            ExprData::Dispatch {
                slf,
                method_name,
                args,
            } => {
                slf.analyze(ct, env, cls)?;
                let callee_type = if slf.stype == sym("SELF_TYPE") {
                    cls
                } else {
                    &slf.stype
                };

                let ((params, mut return_type), _) =
                    ct.get_method_dynamic(callee_type, method_name)?;
                if return_type == "SELF_TYPE" {
                    return_type = slf.stype.clone();
                }
                if args.len() != params.len() {
                    let msg = format!(
                        "In call to method {} at {:?}, method has {} \
                        parameters but received {} arguments.",
                        method_name,
                        span,
                        params.len(),
                        args.len()
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                for (next_param, arg) in args.iter_mut().enumerate() {
                    arg.analyze(ct, env, cls)?;
                    if ct
                        .assert_subtype(&arg.stype, &params[next_param].typ, cls)
                        .is_err()
                    {
                        let msg = format!(
                            "In call to method {} at {:?}, parameter {} is of type  {}, but was passed an argument of type {}",
                            method_name, span, params[next_param].name, params[next_param].typ, arg.stype
                        );
                        return Err(SemanticAnalysisError { msg });
                    }
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
                if ct.assert_subtype(&slf.stype, typ, cls).is_err() {
                    let msg = format!(
                        "In call to {} at {:?}, static dispatch type declared to be {}, \
                        but found self expression of non sub-type {}",
                        method_name, span, typ, slf.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }

                let ((params, mut return_type), _) = ct.get_method(typ, method_name)?;
                if return_type == sym("SELF_TYPE") {
                    return_type = slf.stype.clone();
                }

                if args.len() != params.len() {
                    let msg = format!(
                        "In call to method {} at {:?}, method has {} \
                        parameters but received {} arguments.",
                        method_name,
                        span,
                        params.len(),
                        args.len()
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                for (next_param, arg) in args.iter_mut().enumerate() {
                    arg.analyze(ct, env, cls)?;
                    if ct
                        .assert_subtype(&arg.stype, &params[next_param].typ, cls)
                        .is_err()
                    {
                        let msg = format!(
                            "In call to method {} at {:?}, parameter {} is of type  {}, but was passed an argument of type {}",
                            method_name, span, params[next_param].name, params[next_param].typ, arg.stype
                        );
                        return Err(SemanticAnalysisError { msg });
                    }
                }

                stype = return_type;
            }

            ExprData::TypCase { expr: _, cases } => {
                let mut iter = cases.iter_mut();
                let mut lub = match iter.next() {
                    None => panic!(
                        "{:?}:  Got a typecase expression with no branches.
                    This should not happen.",
                        span
                    ),
                    Some(branch) => {
                        branch.expr.analyze(ct, env, cls)?;
                        branch.expr.stype.clone()
                    }
                };

                for case in iter {
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
                if init.stype != sym("No_type") && ct.assert_subtype(&init.stype, typ, cls).is_err()
                {
                    let msg = format!(
                        "Let binding variable at {:?} declared to be of type {}, 
                        but found initializer of non sub-type {}",
                        span, typ, init.stype
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
                if pred.stype != sym("Bool") {
                    let msg = format!(
                        "While-loop predicate at {:?} is of non-Bool type {}",
                        span, pred.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                body.analyze(ct, env, cls)?;
                stype = sym("Object");
            }

            ExprData::Cond {
                pred,
                then_expr,
                else_expr,
            } => {
                pred.analyze(ct, env, cls)?;
                if pred.stype != sym("Bool") {
                    let msg = format!(
                        "Conditional predicate at {:?} is of non-Bool type {}",
                        span, pred.stype
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
                            "Empty block expression at {:?}
                            This should have been caught by the parser.",
                            span
                        )
                    }
                }
            }

            ExprData::Assign { id, expr } => {
                expr.analyze(ct, env, cls)?;
                match env.lookup(id) {
                    None => {
                        let msg =
                            format!("At {:?}: No object named \"{}\" found in scope.", span, id);
                        return Err(SemanticAnalysisError { msg });
                    }
                    Some(var_type) => {
                        if ct.assert_subtype(&expr.stype, &var_type, cls).is_err() {
                            let msg = format!(
                                "Assignment at {:?} to variable {} previously declared to 
                                be of type {}, but assigned to non sub-type {}",
                                span, id, var_type, expr.stype
                            );
                            return Err(SemanticAnalysisError { msg });
                        }
                        stype = var_type;
                    }
                }
            }
            ExprData::NoExpr {} => {
                stype = sym("No_type");
            }
            ExprData::IntConst { val: _ } => {
                stype = sym("Int");
            }
            ExprData::StrConst { val: _ } => {
                stype = sym("String");
            }
            ExprData::BoolConst { val: _ } => {
                stype = sym("Bool");
            }
            ExprData::IsVoid { expr } => {
                expr.analyze(ct, env, cls)?;
                stype = sym("Bool");
            }
            ExprData::New { typ } => {
                if ct.native_classes.contains(typ) || ct.program_classes.contains(typ) {
                    stype = typ.clone();
                } else {
                    let msg = format!(
                        "New expression at {:?} references undefined type {}",
                        span, typ
                    );
                    return Err(SemanticAnalysisError { msg });
                }
            }
            ExprData::Not { expr } => {
                expr.analyze(ct, env, cls)?;
                if expr.stype != sym("Bool") {
                    let msg = format!(
                        "Argument to not at {:?} was of non-Bool type {}",
                        span, expr.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = sym("Bool");
            }
            ExprData::Comp { expr } => {
                expr.analyze(ct, env, cls)?;
                if expr.stype != sym("Int") {
                    let msg = format!(
                        "Argument to ~ at {:?} was of non-Int type {}",
                        span, expr.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = sym("Int");
            }
            ExprData::Lt { lhs, rhs } | ExprData::Leq { lhs, rhs } => {
                lhs.analyze(ct, env, cls)?;
                if lhs.stype != sym("Int") {
                    let msg = format!(
                        "Left-hand side of operator at {:?} was of non-Int type {}",
                        span, lhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                rhs.analyze(ct, env, cls)?;
                if rhs.stype != sym("Int") {
                    let msg = format!(
                        "Right-hand side of operator at {:?} was of non-Int type {}",
                        span, rhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = sym("Bool");
            }
            ExprData::Eq { lhs, rhs } => {
                // Unlike < and <=, the = operator can take arbitrary types.
                lhs.analyze(ct, env, cls)?;
                rhs.analyze(ct, env, cls)?;

                let mut comp_as_type = sym("Object");
                if (lhs.stype == sym("Int"))
                    | (lhs.stype == sym("String"))
                    | (lhs.stype == sym("Bool"))
                {
                    comp_as_type = lhs.stype.clone();
                }
                if (rhs.stype == sym("Int"))
                    | (rhs.stype == sym("String"))
                    | (rhs.stype == sym("Bool"))
                {
                    comp_as_type = rhs.stype.clone();
                }
                if comp_as_type != sym("Object") && lhs.stype != rhs.stype {
                    let msg = format!(
                        "Equality operands at {:?} had incompatible types lhs: {} and rhs: {}.",
                        span, lhs.stype, rhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }

                stype = sym("Bool");
            }

            ExprData::Plus { lhs, rhs }
            | ExprData::Minus { lhs, rhs }
            | ExprData::Times { lhs, rhs }
            | ExprData::Divide { lhs, rhs } => {
                lhs.analyze(ct, env, cls)?;
                if lhs.stype != sym("Int") {
                    let msg = format!(
                        "Left-hand side of operator at {:?} was of non-Int type {}",
                        span, lhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                rhs.analyze(ct, env, cls)?;
                if rhs.stype != sym("Int") {
                    let msg = format!(
                        "Right-hand side of operator at {:?} was of non-Int type {}",
                        span, rhs.stype
                    );
                    return Err(SemanticAnalysisError { msg });
                }
                stype = sym("Int");
            }
            ExprData::Object { id } => match env.lookup(id) {
                None => {
                    let msg = format!("At {:?}: No object named \"{}\" found in scope.", span, id);
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
    use crate::ast_parse::Parse;

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
        let result = f1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut f2 = Feature::parse(c2).unwrap();
        let result = f2.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut f3 = Feature::parse(c3).unwrap();
        let result = f3.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut f4 = Feature::parse(c4).unwrap();
        let result = f4.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut desired_env = Env::new();
        desired_env.add_binding(&sym("a"), &sym("Int"));
        desired_env.add_binding(&sym("b"), &sym("Banana"));
        desired_env.add_binding(&sym("c"), &sym("Banana"));
        assert_eq!(env, desired_env);
    }

    #[test]
    fn test_semant_features_wrong_types() {
        let c1: &str = r#"
        a: Int <- "hello";
        "#;

        let c2: &str = r"
        foo(): Banana {42};
        ";

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let mut f1 = Feature::parse(c1).unwrap();
        let result = f1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert!(result.is_err());

        let mut f2 = Feature::parse(c2).unwrap();
        let result = f2.analyze(&ct, &mut env, &sym("UNUSED"));
        assert!(result.is_err());
    }

    #[test]
    fn test_semant_program_calls_analyze_down_to_exprs() {
        let c1: &str = r"
        class ABC {
        a: Int <- 42;
        b: Banana;
        c: Banana <- b;
        foo(): Banana {c};
        };
        ";

        let mut program = Program::parse(c1).unwrap();
        program.semant().unwrap();

        let cls = program.classes.first().unwrap();

        let f1 = cls.features.first().unwrap();
        let f4 = cls.features.get(3).unwrap();

        if let Feature::Attr {
            name: _,
            typ: _,
            init,
        } = f1
        {
            assert_eq!(init.stype, sym("Int"));
        } else {
            panic!("unreachable");
        }

        if let Feature::Method {
            name: _,
            formals: _,
            typ: _,
            body,
        } = f4
        {
            assert_eq!(body.stype, sym("Banana"));
        } else {
            panic!("unreachable");
        }
    }

    #[test]
    fn test_semant_dispatch() {
        let cls_cd: &str = r"
        class Apple {foo(): Banana {new Banana};};
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
        let result = attr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, sym("Banana"));
    }

    #[test]
    fn test_semant_dispatch_wrong_number_of_args() {
        let cls_cd: &str = r"
        class Apple {foo(x: Int): Int {42};};
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
        let result = attr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert!(result.is_err());
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
        let result = attr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, sym("Banana"));
    }

    #[test]
    fn test_semant_static_dispatch_wrong_number_of_args() {
        let cls_cd: &str = r"
        class Apple {foo(x: Int): Int {42};};
        ";
        let attr_cd: &str = r"
        a: Apple;
        ";
        let expr_cd: &str = r"
        a@Apple.foo(1, 2)
        ";

        let cls = Class::parse(cls_cd).unwrap();
        let ct = ClassTable::new(&vec![cls]).unwrap();
        let mut env = Env::new();

        let mut attr = Feature::parse(attr_cd).unwrap();
        let result = attr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert!(result.is_err());
    }

    #[test]
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
        let result = attr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, sym("Banana"));
    }

    #[test]
    fn test_semant_dynamic_dispatch_simple_selftype() {
        let code: &str = r"
            class A inherits IO {
                init() : SELF_TYPE {self};
            };
        ";

        let mut cls = Class::parse(code).unwrap();
        let ct = ClassTable::new(&vec![cls.to_owned()]).unwrap();
        let mut env = Env::new();

        let result = cls.semant(&ct, &mut env);
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn test_semant_dynamic_dispatch_selftype() {
        let cls_cd_1: &str = r"
        class Apple {foo(): SELF_TYPE {new SELF_TYPE};};
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
        let result = attr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, sym("Kiwi"));
    }

    #[test]
    fn test_semant_dynamic_dispatch_selftype_self() {
        let cls_cd_1: &str = r"
        class Apple {foo(): SELF_TYPE {self};};
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
        let result = attr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut expr = Expr::parse(expr_cd).unwrap();
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        assert_eq!(expr.stype, sym("Kiwi"));
    }

    #[test]
    fn test_semant_dynamic_dispatch_on_self() {
        let code: &str = r"
            class A inherits IO {
                foo() : Int {self.in_int()};
            };
        ";

        let mut cls = Class::parse(code).unwrap();
        let ct = ClassTable::new(&vec![cls.to_owned()]).unwrap();
        let mut env = Env::new();

        let result = cls.semant(&ct, &mut env);
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn test_semant_exprs_int() {
        let c1: &str = r"
        42
        ";
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, sym("Int"));
    }

    #[test]
    fn test_semant_let() {
        let c1: &str = r"
        let x: Int in x * x
        ";

        let c2: &str = r"
        let x: String in (new IO).out_string(x)
        ";

        let mut e1 = Expr::parse(c1).unwrap();
        let mut e2 = Expr::parse(c2).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result1 = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result1, Ok(()));
        let result2 = e2.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result2, Ok(()));

        assert_eq!(e1.stype, sym("Int"));
        assert_eq!(e2.stype, sym("IO"));
    }

    #[test]
    fn test_semant_let_bad_type() {
        let c1: &str = r"
        let x: Int in not x 
        ";

        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result1 = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert!(result1.is_err());
    }

    #[test]
    fn test_semant_typcase() {
        let cls_code: &str = r"
        class Banana {};
        class Apple inherits Banana {};
        class Orange inherits Banana {};
        ";

        let expr_code: &str = r"
        case 42 of
            a: T1 => new Apple;
            b: T2 => new Orange;
        esac
        ";

        let program = Program::parse(cls_code).unwrap();
        let ct = ClassTable::new(&program.classes).unwrap();

        let mut expr = Expr::parse(expr_code).unwrap();

        let mut env = Env::new();
        let result1 = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result1, Ok(()));
        assert_eq!(expr.stype, sym("Banana"));
    }

    #[test]
    fn test_semant_exprs_str() {
        let c1: &str = r#"
        "42"
        "#;
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, sym("String"));
    }
    #[test]
    fn test_semant_exprs_bool() {
        let c1: &str = "
        true
        ";
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, sym("Bool"));
    }

    #[test]
    fn test_semant_exprs_plus() {
        let c1: &str = r"
        42 + 2
        ";
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, sym("Int"));
    }

    #[test]
    fn test_semant_exprs_plus_string() {
        let c1: &str = r#"
        42 + "hello"
        "#;
        let mut e1 = Expr::parse(c1).unwrap();

        let mut env = Env::new();
        let ct = ClassTable::new(&vec![]).unwrap();

        let result = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert!(result.is_err());
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
        let result = f1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut f2 = Feature::parse(c2).unwrap();
        let result = f2.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let c1: &str = r#"
        a
        "#;
        let c2: &str = r#"
        b
        "#;
        let c3: &str = r#"
        c
        "#;
        let mut e1 = Expr::parse(c1).unwrap();
        let mut e2 = Expr::parse(c2).unwrap();
        let mut e3 = Expr::parse(c3).unwrap();
        let result1 = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        let result2 = e2.analyze(&ct, &mut env, &sym("UNUSED"));
        let result3 = e3.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result1, Ok(()));
        assert_eq!(e1.stype, sym("Apple"));
        assert_eq!(result2, Ok(()));
        assert_eq!(e2.stype, sym("Banana"));
        assert!(result3.is_err());
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
        let result = f1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut f2 = Feature::parse(c2).unwrap();
        let result = f2.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let mut f3 = Feature::parse(c3).unwrap();
        let result = f3.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let code: &str = r#"
        if c then a else b fi
        "#;
        let mut expr = Expr::parse(code).expect("Test code failed to parse");
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));
        assert_eq!(expr.stype, sym("Object"));

        let code: &str = r#"
        if a then a else b fi
        "#;
        let mut expr = Expr::parse(code).expect("Test code failed to parse");
        let result = expr.analyze(&ct, &mut env, &sym("UNUSED"));
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

        let result = e1.analyze(&ct, &mut env, &sym("UNUSED"));
        assert_eq!(result, Ok(()));
        assert_eq!(e1.stype, sym("Int"));
    }

    #[test]
    fn test_semant_undefined_method() {
        let code: &str = r#"
        class B {
        x: B;
        bar() : B {x.foo()};
        };
        "#;
        let mut program = Program::parse(code).expect("Test code failed to parse");
        let result = program.semant();
        assert!(result.is_err());
    }
}
