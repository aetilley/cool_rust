use crate::ast::Classes;
use crate::semant::SemanticAnalysisError;

pub struct ClassTable {
    classes: Classes,
}

impl ClassTable {
    pub fn new(classes: Classes) -> Result<ClassTable, SemanticAnalysisError> {
        Ok(Self {
            classes: classes.clone(),
        })
    }

    pub fn get_param_types(&self, class_name: &str, method_name: &str) -> Vec<String> {
        vec![]
    }
    pub fn get_return_type(&self, class_name: &str, method_name: &str) -> String {
        "Object".to_owned()
    }

    pub fn get_lub(&self, t1: &str, t2: &str) -> String {
        // TODO!
        "Object".to_owned()

    } 
    pub fn assert_subtype(&self, t1: &str, t2: &str) -> Result<(), SemanticAnalysisError> {
        // TODO!
        if t1 == "No_type" {return Ok(())}
        if t2 == "Object" {return Ok(())}

        Ok(())
    }
}
