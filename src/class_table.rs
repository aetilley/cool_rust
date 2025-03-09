use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::{Class, Classes, Feature, Formals};
use crate::semant::SemanticAnalysisError;

type ClassParentTy = HashMap<String, String>;
type ClassChildrenTy = HashMap<String, Vec<String>>;
type ClassMethodParamTypesTy = HashMap<String, HashMap<String, Formals>>;
type ClassMethodReturnTypeTy = HashMap<String, HashMap<String, String>>;

pub struct ClassTable {
    basic_classes: Classes,
    classes: Classes,
    class_parent: ClassParentTy,
    class_children: ClassChildrenTy,
    class_method_param_types: ClassMethodParamTypesTy,
    class_method_return_type: ClassMethodReturnTypeTy,
}

impl ClassTable {
    fn create_native_classes() -> Classes {
        // TODO
        vec![]
    }

    fn _check_descendants_for_cycles(
        node: &str,
        visited: &mut HashSet<String>,
        class_children: &ClassChildrenTy,
    ) -> Result<(), SemanticAnalysisError> {
        for child in class_children.get(node).unwrap().iter() {
            if visited.contains(child) {
                let msg = format!(
                    "When visiting children of type {}, encountered previously visited type {}",
                    node, child
                );
                return Err(SemanticAnalysisError { msg });
            }
            visited.insert(child.to_owned());
            ClassTable::_check_descendants_for_cycles(child, visited, class_children)?;
        }
        Ok(())
    }

    fn detect_cycles(class_children: &ClassChildrenTy) -> Result<(), SemanticAnalysisError> {
        let mut root = "Object";
        let mut visited = HashSet::<String>::new();
        visited.insert(root.to_owned());
        ClassTable::_check_descendants_for_cycles(root, &mut visited, class_children)
    }

    fn register_methods_and_relatives(
        class: &Class,
        class_method_param_types: &mut ClassMethodParamTypesTy,
        class_method_return_type: &mut ClassMethodReturnTypeTy,
        class_parent: &mut ClassParentTy,
        class_children: &mut ClassChildrenTy,
    ) {
        class_parent.insert(class.name.clone(), class.parent.clone());
        class_children
            .entry(class.parent.clone())
            .and_modify(|children| children.push(class.name.clone()))
            .or_insert(vec![class.name.clone()]);

        let mut method_param_types = HashMap::<String, Formals>::new();
        let mut method_return_type = HashMap::<String, String>::new();

        for feature in class.features.iter() {
            if let Feature::Method {
                name,
                formals,
                typ,
                body,
            } = feature
            {
                method_param_types.insert(name.clone(), formals.clone());
                method_return_type.insert(name.clone(), typ.clone());
            }
        }

        class_method_param_types.insert(class.name.clone(), method_param_types);
        class_method_return_type.insert(class.name.clone(), method_return_type);
    }

    pub fn new(classes: &Classes) -> Result<ClassTable, SemanticAnalysisError> {
        let classes = classes.clone();
        let basic_classes = ClassTable::create_native_classes();
        let mut class_parent = HashMap::<String, String>::new();
        let mut class_children = HashMap::<String, Vec<String>>::new();
        let mut class_method_param_types = HashMap::<String, HashMap<String, Formals>>::new();
        let mut class_method_return_type = HashMap::<String, HashMap<String, String>>::new();

        for class in basic_classes.iter() {
            ClassTable::register_methods_and_relatives(
                class,
                &mut class_method_param_types,
                &mut class_method_return_type,
                &mut class_parent,
                &mut class_children,
            );
        }

        for class in classes.iter() {
            ClassTable::register_methods_and_relatives(
                class,
                &mut class_method_param_types,
                &mut class_method_return_type,
                &mut class_parent,
                &mut class_children,
            );
        }

        ClassTable::detect_cycles(&class_children)?;

        Ok(ClassTable {
            basic_classes,
            classes,
            class_parent,
            class_children,
            class_method_param_types,
            class_method_return_type,
        })
    }

    pub fn get_param_types(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Result<Formals, SemanticAnalysisError> {
        match self.class_method_param_types.get(class_name) {
            None => {
                let msg = format!("No methods found for class {}.", class_name);
                Err(SemanticAnalysisError { msg })
            }
            Some(methods) => match methods.get(method_name) {
                None => {
                    let msg = format!("No method {} found for class {}.", method_name, class_name);
                    Err(SemanticAnalysisError { msg })
                }
                Some(params) => Ok(params.clone()),
            },
        }
    }

    pub fn get_return_type(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Result<String, SemanticAnalysisError> {
        match self.class_method_return_type.get(class_name) {
            None => {
                let msg = format!("No methods found for class {}.", class_name);
                Err(SemanticAnalysisError { msg })
            }
            Some(methods) => match methods.get(method_name) {
                None => {
                    let msg = format!("No method {} found for class {}.", method_name, class_name);
                    Err(SemanticAnalysisError { msg })
                }
                Some(return_type) => Ok(return_type.clone()),
            },
        }
    }

    pub fn get_lub(&self, t1: &str, t2: &str) -> String {
        // In order to comput the least upper bound of two types, we
        // comput the two ancestries to the root "Object" and then
        // read them both backwards to find the first place they diverge.

        let mut ancestry1 = vec![t1];
        let mut ancestry2 = vec![t2];
        let mut next = t1.clone();
        while let Some(parent) = self.class_parent.get(next) {
            ancestry1.push(parent);
            next = parent;
        }
        let mut next = t2.clone();
        while let Some(parent) = self.class_parent.get(t2) {
            ancestry1.push(parent);
            next = parent;
        }
        let mut rev_pairs = ancestry1.into_iter().rev().zip(ancestry2.into_iter().rev());
        let (e1, e2) = rev_pairs.next().unwrap();
        if e1 != e2 || e1 != "Object" {
            panic!("Ancestries somehow doen't both start at type Object");
        }
        let mut lub = e1;
        while let Some((e1, e2)) = rev_pairs.next() {
            if e1 != e2 {
                return lub.to_owned();
            }
            lub = e1
        }
        return lub.to_owned();
    }

    pub fn assert_subtype(&self, t1: &str, t2: &str) -> Result<(), SemanticAnalysisError> {
        if t1 == t2 {
            return Ok(());
        }

        let mut next = t1.clone();
        while let Some(parent) = self.class_parent.get(next) {
            if parent == t2 {
                return Ok(());
            };
            next = parent;
        }

        let msg = format!("Type {} is not a subtype of type {}", t1, t2);
        return Err(SemanticAnalysisError { msg });
    }
}
