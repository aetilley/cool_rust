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
}
