use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::Expr;
use crate::ast::{Class, Classes, Feature, Formal, Formals};
use crate::semant::SemanticAnalysisError;

use common_macros::hash_set;

type ClassParentTy = HashMap<String, String>;
type ClassChildrenTy = HashMap<String, HashSet<String>>;
type ClassMethodParamTypesTy = HashMap<String, HashMap<String, Formals>>;
type ClassMethodReturnTypeTy = HashMap<String, HashMap<String, String>>;

#[derive(Debug, PartialEq)]
pub struct ClassTable {
    native_classes: HashSet<String>,
    classes: HashSet<String>,
    class_parent: ClassParentTy,
    class_children: ClassChildrenTy,
    class_method_param_types: ClassMethodParamTypesTy,
    class_method_return_type: ClassMethodReturnTypeTy,
}

impl ClassTable {
    fn create_native_classes() -> Classes {
        // The bodies below are empty, since they will be supplied by the runtime.
        let mut native_classes: Classes = vec![];

        // Object
        let object_features = vec![
            Feature::method("abort", vec![], "Object", Expr::no_expr()),
            Feature::method("type_name", vec![], "Str", Expr::no_expr()),
            Feature::method("copy", vec![], "SELF_TYPE", Expr::no_expr()),
        ];
        let object_class = Class::class("Object", "No_class", object_features);
        native_classes.push(object_class);

        // IO
        let io_features = vec![
            Feature::method(
                "out_string",
                vec![Formal::formal("arg", "Str")],
                "SELF_TYPE",
                Expr::no_expr(),
            ),
            Feature::method(
                "out_int",
                vec![Formal::formal("arg", "Int")],
                "SELF_TYPE",
                Expr::no_expr(),
            ),
            Feature::method("in_string", vec![], "Str", Expr::no_expr()),
            Feature::method("in_int", vec![], "Int", Expr::no_expr()),
        ];
        let io_class = Class::class("IO", "Object", io_features);
        native_classes.push(io_class);

        // Int
        let int_features = vec![Feature::attr("val", "prim_slot", Expr::no_expr())];
        let int_class = Class::class("Int", "Object", int_features);
        native_classes.push(int_class);

        // Bool
        let bool_features = vec![Feature::attr("val", "prim_slot", Expr::no_expr())];
        let bool_class = Class::class("Bool", "Object", bool_features);
        native_classes.push(bool_class);

        // Str
        let str_features = vec![
            Feature::attr("val", "Int", Expr::no_expr()),
            Feature::attr("str_field", "prim_slot", Expr::no_expr()),
            Feature::method("length", vec![], "Int", Expr::no_expr()),
            Feature::method(
                "concat",
                vec![Formal::formal("arg", "Str")],
                "Str",
                Expr::no_expr(),
            ),
            Feature::method(
                "substr",
                vec![Formal::formal("arg", "Int"), Formal::formal("arg2", "Int")],
                "Str",
                Expr::no_expr(),
            ),
        ];
        let str_class = Class::class("Str", "Object", str_features);
        native_classes.push(str_class);

        native_classes
    }

    fn _mark_descendants(
        node: &str,
        visited: &mut HashSet<String>,
        class_children: &ClassChildrenTy,
    ) -> Result<(), SemanticAnalysisError> {
        match class_children.get(node) {
            None => Ok(()),
            Some(children) => {
                for child in children.iter() {
                    if visited.contains(child) {
                        let msg = format!(
                            "When visiting children of type {}, encountered previously visited type {}",
                            node, child
                        );
                        return Err(SemanticAnalysisError { msg });
                    }
                    visited.insert(child.to_owned());
                    ClassTable::_mark_descendants(child, visited, class_children)?;
                }
                Ok(())
            }
        }
    }

    fn detect_cycles(class_children: &ClassChildrenTy) -> Result<(), SemanticAnalysisError> {
        let root = "Object";
        let mut visited = HashSet::<String>::new();
        visited.insert(root.to_owned());
        ClassTable::_mark_descendants(root, &mut visited, class_children)
            .expect("We really should not be getting loops involving Object");
        let mut detached_classes = hash_set! {};
        for key in class_children.keys() {
            if !visited.contains::<String>(key) {
                detached_classes.insert(key.clone());
            }
        }
        if !detached_classes.is_empty() {
            let msg = format!(
                "The following classes are not decendants of Object.
               (this is likely due to their being involved in an inheritance cycle).: {:?} ",
                detached_classes
            );
            return Err(SemanticAnalysisError { msg });
        }
        Ok(())
    }

    fn register_methods_and_relatives(
        class: &Class,
        class_method_param_types: &mut ClassMethodParamTypesTy,
        class_method_return_type: &mut ClassMethodReturnTypeTy,
        class_parent: &mut ClassParentTy,
        class_children: &mut ClassChildrenTy,
    ) {
        if class.parent != "No_class" {
            class_parent.insert(class.name.clone(), class.parent.clone());
            class_children
                .entry(class.parent.clone())
                .and_modify(|children| {
                    children.insert(class.name.clone());
                })
                .or_insert(hash_set! {class.name.clone()});
            class_children
                .entry(class.name.clone())
                .or_insert(hash_set! {});
        }

        let mut method_param_types = HashMap::<String, Formals>::new();
        let mut method_return_type = HashMap::<String, String>::new();

        for feature in class.features.iter() {
            if let Feature::Method {
                name,
                formals,
                typ,
                body: _,
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
        let mut class_parent = HashMap::<String, String>::new();
        let mut class_children = HashMap::<String, HashSet<String>>::new();
        let mut class_method_param_types = HashMap::<String, HashMap<String, Formals>>::new();
        let mut class_method_return_type = HashMap::<String, HashMap<String, String>>::new();

        let native_classes = ClassTable::create_native_classes();
        for class in native_classes.iter() {
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

        //let native_class_names = native_classes.map
        let class_names = classes.iter().map(|cls| cls.name.clone()).collect();
        let native_class_names = native_classes.iter().map(|cls| cls.name.clone()).collect();
        Ok(ClassTable {
            native_classes: native_class_names,
            classes: class_names,
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
        let mut next = t1;
        while let Some(parent) = self.class_parent.get(next) {
            ancestry1.push(parent);
            next = parent;
        }
        let mut next = t2;
        while let Some(parent) = self.class_parent.get(next) {
            ancestry2.push(parent);
            next = parent;
        }
        let mut rev_pairs = ancestry1.into_iter().rev().zip(ancestry2.into_iter().rev());
        let (e1, e2) = rev_pairs.next().unwrap();
        if e1 != e2 || e1 != "Object" {
            panic!("Ancestries somehow doen't both start at type Object");
        }
        let mut lub = e1;
        for (e1, e2) in rev_pairs {
            if e1 != e2 {
                return lub.to_owned();
            }
            lub = e1
        }
        lub.to_owned()
    }

    pub fn assert_subtype(&self, t1: &str, t2: &str) -> Result<(), SemanticAnalysisError> {
        // If this function gets called on "No_type" almost certainly something has
        // gone wrong.
        assert_ne!(t1, "No_type");
        assert_ne!(t2, "No_type");

        if t1 == t2 {
            return Ok(());
        }

        let mut next = t1;
        while let Some(parent) = self.class_parent.get(next) {
            if parent == t2 {
                return Ok(());
            };
            next = parent;
        }

        let msg = format!("Type {} is not a subtype of type {}", t1, t2);
        Err(SemanticAnalysisError { msg })
    }
}

#[cfg(test)]
mod class_table_tests {

    use super::*;
    use crate::ast::{Parse, Program};
    use common_macros::hash_map;

    #[test]
    fn test_assert_subtype() {
        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = "Int";
        let t2 = "Int";
        let result = ct.assert_subtype(t1, t2);
        assert_eq!(result, Ok(()));

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = "Int";
        let t2 = "Object";
        let result = ct.assert_subtype(t1, t2);
        assert_eq!(result, Ok(()));

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = "Object";
        let t2 = "Int";
        let result = ct.assert_subtype(t1, t2);
        assert!(result.is_err());
    }

    #[test]
    fn test_constructor() {
        let code: &str = r"
        class Apple {};
        class Orange inherits Apple {
            foo() : T2 {42};
            bar(c: S1, d: S2) : S3 {true};
        };
        ";
        let program = Program::parse(code).unwrap();

        let result = ClassTable::new(&program.classes).unwrap();

        let desired_classes = hash_set! {
            "Apple".to_owned(),
            "Orange".to_owned(),
        };

        let desired_native_classes = hash_set! {
            "Int".to_owned(),
            "IO".to_owned(),
            "Bool".to_owned(),
            "Str".to_owned(),
            "Object".to_owned(),
        };

        let desired_class_parent = hash_map! {
            "Int".to_owned()=>"Object".to_owned(),
            "IO".to_owned()=>"Object".to_owned(),
            "Bool".to_owned()=>"Object".to_owned(),
            "Str".to_owned()=>"Object".to_owned(),
            "Apple".to_owned()=>"Object".to_owned(),
            "Orange".to_owned()=>"Apple".to_owned(),
        };

        let desired_class_children = hash_map! {
            "Object".to_owned() => hash_set!{
                "IO".to_owned(),
                "Str".to_owned(),
                "Bool".to_owned(),
                "Int".to_owned(),
                "Apple".to_owned(),
            },
            "Apple".to_owned() => hash_set!{
                "Orange".to_owned(),
            },
        };
        let desired_class_method_param_types = hash_map! {
            "Object".to_owned() => hash_map!{
                "abort".to_owned() => vec![],
                "type_name".to_owned() => vec![],
                "copy".to_owned() => vec![],
            },
            "IO".to_owned() => hash_map!{
                "out_string".to_owned() => vec![Formal::formal("arg", "Str")],
                "out_int".to_owned() => vec![Formal::formal("arg", "Int")],
                "in_string".to_owned() => vec![],
                "in_int".to_owned() => vec![],
            },
            "Str".to_owned() => hash_map!{
                "length".to_owned() => vec![],
                "concat".to_owned() => vec![Formal::formal("arg", "Str")],
                "substr".to_owned() => vec![Formal::formal("arg", "Int"), Formal::formal("arg2", "Int")],
            },
            "Bool".to_owned() => hash_map!{},
            "Int".to_owned() => hash_map!{},

            "Apple".to_owned() => hash_map!{},
            "Orange".to_owned() => hash_map!{
                "foo".to_owned() => vec![],
                "bar".to_owned() => vec![Formal::formal("c", "S1"), Formal::formal("d", "S2")],
            },
        };

        let desired_class_method_return_types = hash_map! {
            "Object".to_owned() => hash_map!{
                "abort".to_owned() => "Object".to_owned(),
                "type_name".to_owned() => "Str".to_owned(),
                "copy".to_owned() => "SELF_TYPE".to_owned(),
            },
            "IO".to_owned() => hash_map!{
                "out_string".to_owned() => "SELF_TYPE".to_owned(),
                "out_int".to_owned() => "SELF_TYPE".to_owned(),
                "in_string".to_owned() => "Str".to_owned(),
                "in_int".to_owned() => "Int".to_owned(),
            },
            "Str".to_owned() => hash_map!{
                "length".to_owned() => "Int".to_owned(),
                "concat".to_owned() => "Str".to_owned(),
                "substr".to_owned() => "Str".to_owned(),
            },
            "Bool".to_owned() => hash_map!{},
            "Int".to_owned() => hash_map!{},

            "Apple".to_owned() => hash_map!{},
            "Orange".to_owned() => hash_map!{
                "foo".to_owned() => "T2".to_owned(),
                "bar".to_owned() => "S3".to_owned(),
            },
        };

        assert_eq!(result.classes, desired_classes);
        assert_eq!(result.native_classes, desired_native_classes);
        assert_eq!(result.class_parent, desired_class_parent);
        assert_eq!(result.class_children, desired_class_children);
        assert_eq!(
            result.class_method_param_types,
            desired_class_method_param_types
        );
        assert_eq!(
            result.class_method_return_type,
            desired_class_method_return_types
        );
    }

    #[test]
    fn test_detect_cycles() {
        let code: &str = r"
        class Apple inherits Kiwi {};
        class Orange inherits Apple {};
        class Lemon inherits Orange {};
        class Grape inherits Lemon {};
        class Kiwi inherits Orange {};
        ";
        let program = Program::parse(code).unwrap();
        let result = ClassTable::new(&program.classes);
        assert!(result.is_err());
    }

    //#[test]
    fn test_detect_get_return_type() {
        assert!(false);
    }

    //#[test]
    fn test_detect_get_param_types() {
        assert!(false);
    }

    #[test]
    fn test_get_lub() {
        let code: &str = r"
        class Apple {};
        class Orange inherits Apple {};
        class Lemon inherits Orange {};
        class Grape inherits Lemon {};
        class Kiwi inherits Orange {};
        ";
        let program = Program::parse(code).unwrap();
        let ct = ClassTable::new(&program.classes).unwrap();
        let result = ct.get_lub("Kiwi", "Grape");
        assert_eq!(result, "Orange");
    }
}
