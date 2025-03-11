use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::Expr;
use crate::ast::{Class, Classes, Feature, Formal, Formals};
use crate::semant::SemanticAnalysisError;

use crate::symbol::{sym, Sym};

use common_macros::hash_set;

type ClassParentTy = HashMap<Sym, Sym>;
type ClassChildrenTy = HashMap<Sym, HashSet<Sym>>;
type Signature = (Formals, Sym);
type ClassMethodSignatureTy = HashMap<Sym, HashMap<Sym, Signature>>;

#[derive(Debug, PartialEq)]
pub struct ClassTable {
    native_classes: HashSet<Sym>,
    classes: HashSet<Sym>,
    class_parent: ClassParentTy,
    class_children: ClassChildrenTy,
    class_method_signature: ClassMethodSignatureTy,
}

impl ClassTable {
    fn create_native_classes() -> Classes {
        // The bodies below are empty, since they will be supplied by the runtime.
        let mut native_classes: Classes = vec![];

        // Object
        let object_features = vec![
            Feature::method("abort", vec![], "Object", Expr::no_expr()),
            Feature::method("type_name", vec![], "String", Expr::no_expr()),
            Feature::method("copy", vec![], "SELF_TYPE", Expr::no_expr()),
        ];
        let object_class = Class::class("Object", "No_class", object_features);
        native_classes.push(object_class);

        // IO
        let io_features = vec![
            Feature::method(
                "out_string",
                vec![Formal::formal("arg", "String")],
                "SELF_TYPE",
                Expr::no_expr(),
            ),
            Feature::method(
                "out_int",
                vec![Formal::formal("arg", "Int")],
                "SELF_TYPE",
                Expr::no_expr(),
            ),
            Feature::method("in_string", vec![], "String", Expr::no_expr()),
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
                vec![Formal::formal("arg", "String")],
                "String",
                Expr::no_expr(),
            ),
            Feature::method(
                "substr",
                vec![Formal::formal("arg", "Int"), Formal::formal("arg2", "Int")],
                "String",
                Expr::no_expr(),
            ),
        ];
        let str_class = Class::class("String", "Object", str_features);
        native_classes.push(str_class);

        native_classes
    }

    fn _mark_descendants(
        node: Sym,
        visited: &mut HashSet<Sym>,
        class_children: &ClassChildrenTy,
    ) -> Result<(), SemanticAnalysisError> {
        match class_children.get(&node) {
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
                    visited.insert(child.clone());
                    ClassTable::_mark_descendants(child.clone(), visited, class_children)?;
                }
                Ok(())
            }
        }
    }

    fn detect_cycles(class_children: &ClassChildrenTy) -> Result<(), SemanticAnalysisError> {
        let root = sym("Object");
        let mut visited = HashSet::<Sym>::new();
        visited.insert(root);
        ClassTable::_mark_descendants(root, &mut visited, class_children)
            .expect("We really should not be getting loops involving Object");
        let mut detached_classes = hash_set! {};
        for key in class_children.keys() {
            if !visited.contains::<Sym>(key) {
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
        class_method_signature: &mut ClassMethodSignatureTy,
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

        let mut method_signature = HashMap::<Sym, Signature>::new();

        for feature in class.features.iter() {
            if let Feature::Method {
                name,
                formals,
                typ,
                body: _,
            } = feature
            {
                method_signature.insert(*name, (formals.clone(), *typ));
            }
        }

        class_method_signature.insert(class.name.clone(), method_signature);
    }

    pub fn new(classes: &Classes) -> Result<ClassTable, SemanticAnalysisError> {
        let mut class_parent = HashMap::<Sym, Sym>::new();
        let mut class_children = HashMap::<Sym, HashSet<Sym>>::new();
        let mut class_method_signature = HashMap::<Sym, HashMap<Sym, Signature>>::new();

        let native_classes = ClassTable::create_native_classes();
        for class in native_classes.iter() {
            ClassTable::register_methods_and_relatives(
                class,
                &mut class_method_signature,
                &mut class_parent,
                &mut class_children,
            );
        }

        for class in classes.iter() {
            ClassTable::register_methods_and_relatives(
                class,
                &mut class_method_signature,
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
            class_method_signature,
        })
    }

    pub fn get_signature(
        &self,
        class_name: Sym,
        method_name: Sym,
    ) -> Result<Signature, SemanticAnalysisError> {
        match self.class_method_signature.get(&class_name) {
            None => {
                let msg = format!("No methods found for class {}.", class_name);
                Err(SemanticAnalysisError { msg })
            }
            Some(methods) => match methods.get(&method_name) {
                None => {
                    let msg = format!("No method {} found for class {}.", method_name, class_name);
                    Err(SemanticAnalysisError { msg })
                }
                Some(sig) => Ok(sig.clone()),
            },
        }
    }

    pub fn get_signature_dynamic(
        &self,
        class_name: Sym,
        method_name: Sym,
    ) -> Result<Signature, SemanticAnalysisError> {
        if let Ok(signature) = self.get_signature(class_name, method_name) {
            return Ok(signature);
        }

        let mut next = class_name;
        while let Some(parent) = self.class_parent.get(&class_name) {
            match self.get_signature(next, method_name) {
                Err(_) => {
                    next = parent.clone();
                }
                Ok(signature) => {
                    return Ok(signature);
                }
            }
        }
        let msg = format!(
            "No method {} found for class {} or for any ancestor.",
            method_name, class_name
        );
        Err(SemanticAnalysisError { msg })
    }

    pub fn get_lub(&self, t1: Sym, t2: Sym) -> Sym {
        // In order to comput the least upper bound of two types, we
        // comput the two ancestries to the root "Object" and then
        // read them both backwards to find the first place they diverge.

        let mut ancestry1 = vec![t1];
        let mut ancestry2 = vec![t2];
        let mut next = t1;
        while let Some(parent) = self.class_parent.get(&next) {
            ancestry1.push(parent.clone());
            next = parent.clone();
        }
        let mut next = t2;
        while let Some(parent) = self.class_parent.get(&next) {
            ancestry2.push(parent.clone());
            next = parent.clone();
        }
        let mut rev_pairs = ancestry1.into_iter().rev().zip(ancestry2.into_iter().rev());
        let (e1, e2) = rev_pairs.next().unwrap();
        if e1 != e2 || e1 != "Object" {
            panic!("Ancestries somehow doen't both start at type Object");
        }
        let mut lub = e1;
        for (e1, e2) in rev_pairs {
            if e1 != e2 {
                return lub.clone();
            }
            lub = e1
        }
        lub
    }

    pub fn assert_subtype(&self, t1: Sym, t2: Sym) -> Result<(), SemanticAnalysisError> {
        // If this function gets called on "No_type" almost certainly something has
        // gone wrong.
        assert_ne!(t1, sym("No_type"));
        assert_ne!(t2, sym("No_type"));

        if t1 == t2 {
            return Ok(());
        }

        let mut next = t1;
        while let Some(parent) = self.class_parent.get(&next) {
            if parent == &t2 {
                return Ok(());
            };
            next = parent.clone();
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
        let t1 = sym("Int");
        let t2 = sym("Int");
        let result = ct.assert_subtype(t1, t2);
        assert_eq!(result, Ok(()));

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = sym("Int");
        let t2 = sym("Object");
        let result = ct.assert_subtype(t1, t2);
        assert_eq!(result, Ok(()));

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = sym("Object");
        let t2 = sym("Int");
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
            sym("Apple"),
            sym("Orange"),
        };

        let desired_native_classes = hash_set! {
            sym("Int"),
            sym("IO"),
            sym("Bool"),
            sym("String"),
            sym("Object"),
        };

        let desired_class_parent = hash_map! {
            sym("Int")=>sym("Object"),
            sym("IO")=>sym("Object"),
            sym("Bool")=>sym("Object"),
            sym("String")=>sym("Object"),
            sym("Apple")=>sym("Object"),
            sym("Orange")=>sym("Apple"),
        };

        let desired_class_children = hash_map! {
            sym("Object") => hash_set!{
                sym("IO"),
                sym("String"),
                sym("Bool"),
                sym("Int"),
                sym("Apple"),
            },
            sym("Apple") => hash_set!{
                sym("Orange"),
            },
            sym("Bool") => hash_set!{},
            sym("Int") => hash_set!{},
            sym("String") => hash_set!{},
            sym("IO") => hash_set!{},
            sym("Orange") => hash_set!{},
        };
        let desired_class_method_signature = hash_map! {
            sym("Object") => hash_map!{
                sym("abort") => (vec![], sym("Object")),
                sym("type_name") => (vec![], sym("String")),
                sym("copy") => (vec![], sym("SELF_TYPE")),
            },
            sym("IO") => hash_map!{
                sym("out_string") => (vec![Formal::formal("arg", "String")], sym("SELF_TYPE")),
                sym("out_int") => (vec![Formal::formal("arg", "Int")], sym("SELF_TYPE")),
                sym("in_string") => (vec![], sym("String")),
                sym("in_int") => (vec![], sym("Int")),
            },
            sym("String") => hash_map!{
                sym("length") => (vec![], sym("Int")),
                sym("concat") => (vec![Formal::formal("arg", "String")], sym("String")),
                sym("substr") => (vec![Formal::formal("arg", "Int"), Formal::formal("arg2", "Int")], sym("String")),
            },
            sym("Bool") => hash_map!{},
            sym("Int") => hash_map!{},

            sym("Apple") => hash_map!{},
            sym("Orange") => hash_map!{
                sym("foo") => (vec![], sym("T2")),
                sym("bar") => (vec![Formal::formal("c", "S1"), Formal::formal("d", "S2")], sym("S3")),
            },
        };

        assert_eq!(result.classes, desired_classes);
        assert_eq!(result.native_classes, desired_native_classes);
        assert_eq!(result.class_parent, desired_class_parent);
        assert_eq!(result.class_children, desired_class_children);
        assert_eq!(
            result.class_method_signature,
            desired_class_method_signature
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
        let result = ct.get_lub(sym("Kiwi"), sym("Grape"));
        assert_eq!(result, sym("Orange"));
    }
}
