#[allow(unused_imports)]
use crate::ast::{
    Case, Cases, Class, Classes, Expr, ExprData, Exprs, Feature, Features, Formal, Formals,
    LetBinding, LetBindings, Program,
};
use crate::class_table::ClassTable;
use crate::env::Env;

#[derive(Debug, PartialEq)]
pub struct SemanticAnalysisError {
    msg: String,
}

impl Program {
    pub fn semant(&mut self) -> Result<(), SemanticAnalysisError> {
        // Entry point for semantic analysis.  Takes a mutable ref to self
        // because it will populate the type field on descendant asts.

        let ct = ClassTable::new(self.classes.clone())?;

        let mut env = Env::new();

        for class in self.classes.iter_mut() {
            class.semant(&ct, &mut env)?;
        }

        Ok(())
    }
}

impl Class {
    fn semant(&mut self, ct: &ClassTable, env: &mut Env) -> Result<(), SemanticAnalysisError> {
        env.enter_scope();

        env.add_binding("self", self.name.as_ref());

        for feature in self.features.iter_mut() {
            feature.analyze(ct, env, &self.name.to_owned())?;
        }

        env.exit_scope();
        Ok(())
    }
}

/*
The analysis trait provides functionality to
1) Do scope- and type-checking of Program.
2) Populate the type field of an Expr.
3) Analyze the class inheritance graph of a Program to verify that
there are no cycles.

Every AST type below class-level should implement the Analyze trait.
*/
pub trait Analyze {
    fn analyze(
        &mut self,
        ct: &ClassTable,
        env: &mut Env,
        cls: &str,
    ) -> Result<(), SemanticAnalysisError>;
}

impl Analyze for Expr {
    fn analyze(
        &mut self,
        _ct: &ClassTable,
        env: &mut Env,
        _cls: &str,
    ) -> Result<(), SemanticAnalysisError> {
        let Expr { data, stype } = self;
        let expr_data = &**data;
        match expr_data {
            ExprData::NoExpr {} => {
                *stype = "NoType".to_owned();
            }
            ExprData::IntConst { val: _ } => {
                *stype = "Int".to_owned();
            }
            ExprData::Object { id } => match env.lookup(id) {
                None => {
                    let msg = format!("No object named \"{}\" found in scope.", id);
                    return Err(SemanticAnalysisError { msg });
                }
                Some(value) => *stype = value,
            },

            _ => {
                let msg = format!(
                    "Semantic analysis not yet implemented for variant {:?}",
                    self.data
                );
                return Err(SemanticAnalysisError { msg });
            }
        }

        Ok(())
    }
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
                if init.stype != "NoType" && typ != &init.stype {
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

                if &body.stype != typ {
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

#[cfg(test)]
mod semant_tests {

    use super::*;
    use crate::ast::Parse;

    #[test]
    fn test_semant_simple_program() {
        let code: &str = r"
        class Apple {};
        class Orange inherits Bananas {};
        ";

        let mut program = Program::parse(code).expect("Test code failed to parse");
        let result = program.semant();
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn test_semant_simple_class_1() {
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
        let mut f1 = Feature::parse(c1).unwrap();
        let mut f2 = Feature::parse(c2).unwrap();
        let mut f3 = Feature::parse(c3).unwrap();
        let mut f4 = Feature::parse(c4).unwrap();
        // let mut f3 = Feature::parse(c3).unwrap();
        let ct = ClassTable::new(vec![]).unwrap();
        let mut result;
        result = f1.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        result = f2.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        result = f3.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        result = f4.analyze(&ct, &mut env, "UNUSED");
        assert_eq!(result, Ok(()));
        let mut desired_env = Env::new();
        desired_env.add_binding("a", "Int");
        desired_env.add_binding("b", "Banana");
        desired_env.add_binding("c", "Banana");
        assert_eq!(env, desired_env);
    }
}
