use std::collections::{HashMap, HashSet};

use crate::ast::{Class, Classes, Expr, Feature, Formal, Formals};
use crate::semant::SemanticAnalysisError;

use crate::symbol::{sym, Sym};

use common_macros::hash_set;
use itertools::sorted;

type ClassesTy = HashSet<Sym>;
type ClassParentTy = HashMap<Sym, Sym>;
type ClassChildrenTy = HashMap<Sym, HashSet<Sym>>;
type Signature = (Formals, Sym);
type MethodTy = (Signature, Expr);
type ClassMethodTy = HashMap<Sym, HashMap<Sym, MethodTy>>;
type AttrTypeInit = (Sym, Sym, Expr);
type ClassAttrTy = HashMap<Sym, Vec<AttrTypeInit>>;
type VTableTy = HashMap<Sym, Sym>;
type ClassVTableTy = HashMap<Sym, VTableTy>;
type MethodOrderTy = Vec<Sym>;
type ClassMethodOrderTy = HashMap<Sym, MethodOrderTy>;

#[derive(Debug, PartialEq)]
pub struct ClassTable {
    pub native_classes: ClassesTy,
    pub program_classes: ClassesTy,
    pub class_parent: ClassParentTy,
    pub class_children: ClassChildrenTy,
    pub class_methods: ClassMethodTy,
    pub class_attrs: ClassAttrTy,
    pub class_vtable: ClassVTableTy,
    pub class_method_order: ClassMethodOrderTy,
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
                    visited.insert(*child);
                    ClassTable::_mark_descendants(*child, visited, class_children)?;
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
                detached_classes.insert(*key);
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

    fn register_attrs_methods_and_relatives(
        class: &Class,
        class_methods: &mut ClassMethodTy,
        class_attrs: &mut ClassAttrTy,
        class_parent: &mut ClassParentTy,
        class_children: &mut ClassChildrenTy,
    ) {
        if class.parent != "No_class" {
            class_parent.insert(class.name, class.parent);
            class_children
                .entry(class.parent)
                .and_modify(|children| {
                    children.insert(class.name);
                })
                .or_insert(hash_set! {class.name});
            class_children.entry(class.name).or_insert(hash_set! {});
        }

        let mut attrs = Vec::<AttrTypeInit>::new();
        let mut methods = HashMap::<Sym, MethodTy>::new();

        for feature in class.features.iter() {
            match feature {
                Feature::Method {
                    name,
                    formals,
                    typ,
                    body,
                } => {
                    let signature = (formals.clone(), *typ);
                    let method = (signature, body.clone());
                    methods.insert(*name, method);
                }
                Feature::Attr { name, typ, init } => {
                    attrs.push((*name, *typ, init.clone()));
                }
            }
        }

        class_methods.insert(class.name, methods);
        class_attrs.insert(class.name, attrs);
    }

    fn register_vtable_and_method_offsets_for_class(
        cls: Sym,
        class_parent: &ClassParentTy,
        class_methods: &ClassMethodTy,
        class_vtable: &mut ClassVTableTy,
        class_method_order: &mut ClassMethodOrderTy,
    ) {
        let mut vtable = HashMap::<Sym, Sym>::new();
        let mut method_order = Vec::<Sym>::new();

        // First add all methods for parent.
        if let Some(parent) = class_parent.get(&cls) {
            if !class_vtable.contains_key(parent) {
                ClassTable::register_vtable_and_method_offsets_for_class(
                    *parent,
                    class_parent,
                    class_methods,
                    class_vtable,
                    class_method_order,
                );
            }
            vtable = class_vtable.get(parent).unwrap().clone();
            method_order = class_method_order.get(parent).unwrap().clone();
        } else {
            assert!(
                cls == sym("Object"),
                "Non-Object class {} has no parent.",
                cls
            );
        }

        // Then add methods new to this class:
        // Sorting will make testing simpler.
        let methods = sorted(class_methods.get(&cls).unwrap().keys());
        for method_name in methods {
            vtable.insert(*method_name, cls);

            if method_order.contains(method_name) {
                // Override parent, use same offset.
                let old_index = method_order
                    .iter()
                    .position(|&r| r == *method_name)
                    .unwrap();
                method_order[old_index] = *method_name;
            } else {
                // New function name.
                method_order.push(*method_name);
            }
        }
        class_vtable.insert(cls, vtable);
        class_method_order.insert(cls, method_order);
    }

    pub fn get_max_vtable_size(&self) -> usize {
        self.class_vtable
            .values()
            .map(|vtable| vtable.len())
            .max()
            .unwrap()
    }

    pub fn new(classes: &Classes) -> Result<ClassTable, SemanticAnalysisError> {
        let mut class_parent = HashMap::<Sym, Sym>::new();
        let mut class_children = HashMap::<Sym, HashSet<Sym>>::new();
        let mut class_attrs = HashMap::<Sym, Vec<AttrTypeInit>>::new();
        let mut class_methods = HashMap::<Sym, HashMap<Sym, MethodTy>>::new();

        let program_classes = classes.to_owned();
        let native_classes = ClassTable::create_native_classes();
        let mut all_classes = native_classes.clone();
        all_classes.extend_from_slice(&program_classes[..]);

        for class in all_classes.iter() {
            ClassTable::register_attrs_methods_and_relatives(
                class,
                &mut class_methods,
                &mut class_attrs,
                &mut class_parent,
                &mut class_children,
            );
        }

        ClassTable::detect_cycles(&class_children)?;

        let all_class_names: ClassesTy = all_classes.iter().map(|cls| cls.name).collect();
        let program_class_names: ClassesTy = program_classes.iter().map(|cls| cls.name).collect();
        let native_class_names: ClassesTy = native_classes.iter().map(|cls| cls.name).collect();

        let mut class_vtable = HashMap::<Sym, VTableTy>::new();
        let mut class_method_order = HashMap::<Sym, MethodOrderTy>::new();

        for cls in all_class_names.iter() {
            ClassTable::register_vtable_and_method_offsets_for_class(
                *cls,
                &class_parent,
                &class_methods,
                &mut class_vtable,
                &mut class_method_order,
            );
        }

        //let native_class_names = native_classes.map
        Ok(ClassTable {
            native_classes: native_class_names,
            program_classes: program_class_names,
            class_parent,
            class_children,
            class_methods,
            class_attrs,
            class_vtable,
            class_method_order,
        })
    }

    pub fn get_all_attrs(&self, name: Sym) -> Vec<AttrTypeInit> {
        // Get attrs of class `name` and of all ancestors.
        let mut result = Vec::<AttrTypeInit>::new();
        if let Some(parent) = self.class_parent.get(&name) {
            result = self.get_all_attrs(*parent);
        }
        for attr in self.class_attrs.get(&name).unwrap() {
            result.push(attr.clone())
        }
        result
    }

    pub fn get_method(
        &self,
        class_name: Sym,
        method_name: Sym,
    ) -> Result<MethodTy, SemanticAnalysisError> {
        match self.class_methods.get(&class_name) {
            None => {
                let msg = format!("No methods found for class {}.", class_name);
                Err(SemanticAnalysisError { msg })
            }
            Some(methods) => match methods.get(&method_name) {
                None => {
                    let msg = format!("No method {} found for class {}.", method_name, class_name);
                    Err(SemanticAnalysisError { msg })
                }
                Some(method) => Ok(method.clone()),
            },
        }
    }

    pub fn get_method_dynamic(
        &self,
        class_name: Sym,
        method_name: Sym,
    ) -> Result<MethodTy, SemanticAnalysisError> {
        if let Ok(method) = self.get_method(class_name, method_name) {
            return Ok(method);
        }

        let mut next = class_name;
        while let Some(parent) = self.class_parent.get(&next) {
            next = *parent;
            match self.get_method(next, method_name) {
                Err(_) => {
                    continue;
                }
                Ok(method) => {
                    return Ok(method);
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
            ancestry1.push(*parent);
            next = *parent;
        }
        let mut next = t2;
        while let Some(parent) = self.class_parent.get(&next) {
            ancestry2.push(*parent);
            next = *parent;
        }
        let mut rev_pairs = ancestry1.into_iter().rev().zip(ancestry2.into_iter().rev());
        let (e1, e2) = rev_pairs.next().unwrap();

        if e1 != "Object" {
            panic!("Ancestry of type {} doesn't begin at type Object", t1);
        }
        if e2 != "Object" {
            panic!("Ancestry of type {} doesn't begin at type Object", t2);
        }
        let mut lub = e1;
        for (e1, e2) in rev_pairs {
            if e1 != e2 {
                return lub;
            }
            lub = e1
        }
        lub
    }

    pub fn assert_subtype(&self, t1: Sym, t2: Sym, cls: Sym) -> Result<(), SemanticAnalysisError> {
        // If this function gets called on "No_type" almost certainly something has
        // gone wrong.
        // Note that the class is passed in just in case either of the arguments is SELF_TYPE.
        assert_ne!(t1, sym("No_type"));
        assert_ne!(t2, sym("No_type"));

        let s1 = if t1 == sym("SELF_TYPE") { cls } else { t1 };
        let s2 = if t2 == sym("SELF_TYPE") { cls } else { t2 };

        if s1 == s2 {
            return Ok(());
        }

        let mut next = s1;
        while let Some(parent) = self.class_parent.get(&next) {
            if parent == &s2 {
                return Ok(());
            };
            next = *parent;
        }

        let msg = format!("Type {} is not a subtype of type {}", s1, s2);
        Err(SemanticAnalysisError { msg })
    }
}

#[cfg(test)]
mod class_table_tests {

    use super::*;
    use crate::ast::Program;
    use crate::ast_parse::Parse;
    use common_macros::hash_map;

    #[test]
    fn test_assert_subtype() {
        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = sym("Int");
        let t2 = sym("Int");
        let result = ct.assert_subtype(t1, t2, sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = sym("Int");
        let t2 = sym("Object");
        let result = ct.assert_subtype(t1, t2, sym("UNUSED"));
        assert_eq!(result, Ok(()));

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = sym("Object");
        let t2 = sym("Int");
        let result = ct.assert_subtype(t1, t2, sym("UNUSED"));
        assert!(result.is_err());

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = sym("SELF_TYPE");
        let t2 = sym("Orange");
        let result = ct.assert_subtype(t1, t2, sym("Orange"));
        assert_eq!(result, Ok(()));

        let ct = ClassTable::new(&vec![]).unwrap();
        let t1 = sym("SELF_TYPE");
        let t2 = sym("Int");
        let result = ct.assert_subtype(t1, t2, sym("Orange"));
        assert!(result.is_err());
    }

    #[test]
    fn test_constructor() {
        let code: &str = r"
        class Apple {
            a: Int <- 2 + 3;
            foo(): T2 {41};
        };
        class Orange inherits Apple {
            b: Apple;
            foo() : T2 {42};
            bar(c: S1, d: S2) : S3 {true};
        };
        ";

        let program = Program::parse(code).unwrap();

        let result = ClassTable::new(&program.classes).unwrap();

        let desired_program_classes = hash_set! {
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
        let desired_class_methods = hash_map! {
            sym("Object") => hash_map!{
                sym("abort") => ((vec![], sym("Object")), Expr::no_expr()),
                sym("type_name") => ((vec![], sym("String")), Expr::no_expr()),
                sym("copy") => ((vec![], sym("SELF_TYPE")), Expr::no_expr()),
            },
            sym("IO") => hash_map!{
                sym("out_string") => ((vec![Formal::formal("arg", "String")], sym("SELF_TYPE")), Expr::no_expr()),
                sym("out_int") => ((vec![Formal::formal("arg", "Int")], sym("SELF_TYPE")), Expr::no_expr()),
                sym("in_string") => ((vec![], sym("String")), Expr::no_expr()),
                sym("in_int") => ((vec![], sym("Int")), Expr::no_expr()),
            },
            sym("String") => hash_map!{
                sym("length") => ((vec![], sym("Int")), Expr::no_expr()),
                sym("concat") => ((vec![Formal::formal("arg", "String")], sym("String")), Expr::no_expr()),
                sym("substr") => ((vec![Formal::formal("arg", "Int"), Formal::formal("arg2", "Int")], sym("String")), Expr::no_expr()),
            },
            sym("Bool") => hash_map!{},
            sym("Int") => hash_map!{},

            sym("Apple") => hash_map!{
                sym("foo") => ((vec![], sym("T2")), Expr::int_const("41")),
            },
            sym("Orange") => hash_map!{
                sym("foo") => ((vec![], sym("T2")), Expr::int_const("42")),
                sym("bar") => ((vec![Formal::formal("c", "S1"), Formal::formal("d", "S2")], sym("S3")), Expr::bool_const(true)),
            },


        };

        let desired_class_attrs = hash_map! {
            sym("Object") => vec![],
            sym("IO")=>vec![],
            sym("Int")=>vec![(sym("val"), sym("prim_slot"), Expr::no_expr())],
            sym("Bool")=>vec![(sym("val"), sym("prim_slot"), Expr::no_expr())],
            sym("String")=>vec![(sym("val"), sym("Int"), Expr::no_expr()), (sym("str_field"), sym("prim_slot"), Expr::no_expr())],
            sym("Apple")=>vec![(sym("a"), sym("Int"), Expr::plus(Expr::int_const("2"), Expr::int_const("3")))],
            sym("Orange")=>vec![(sym("b"), sym("Apple"), Expr::no_expr())],
        };

        let desired_class_vtable = hash_map!(
            sym("Object") => hash_map!(
                sym("abort") => sym("Object"),
                sym("type_name") => sym("Object"),
                sym("copy") => sym("Object"),
            ),
            sym("IO") => hash_map!(
                sym("abort") => sym("Object"),
                sym("type_name") => sym("Object"),
                sym("copy") => sym("Object"),
                sym("out_string") => sym("IO"),
                sym("in_string") => sym("IO"),
                sym("out_int") => sym("IO"),
                sym("in_int") => sym("IO"),
            ),
            sym("String") => hash_map!(
                sym("abort") => sym("Object"),
                sym("type_name") => sym("Object"),
                sym("copy") => sym("Object"),
                sym("length") => sym("String"),
                sym("concat") => sym("String"),
                sym("substr") => sym("String"),
            ),
            sym("Bool") => hash_map!(
                sym("abort") => sym("Object"),
                sym("type_name") => sym("Object"),
                sym("copy") => sym("Object"),
            ),
            sym("Int") => hash_map!(
                sym("abort") => sym("Object"),
                sym("type_name") => sym("Object"),
                sym("copy") => sym("Object"),
            ),
            sym("Apple") => hash_map!(
                sym("abort") => sym("Object"),
                sym("type_name") => sym("Object"),
                sym("copy") => sym("Object"),
                sym("foo") => sym("Apple"),
            ),
            sym("Orange") => hash_map!(
                sym("abort") => sym("Object"),
                sym("type_name") => sym("Object"),
                sym("copy") => sym("Object"),
                sym("foo") => sym("Orange"),
                sym("bar") => sym("Orange"),
            ),

        );

        let desired_class_method_order = hash_map!(
            sym("Object") => vec![sym("abort"), sym("copy"), sym("type_name")],
            sym("IO") => vec![sym("abort"), sym("copy"), sym("type_name"), sym("in_int"), sym("in_string"), sym("out_int"), sym("out_string")],
            sym("Bool") => vec![sym("abort"), sym("copy"), sym("type_name")],
            sym("Int") => vec![sym("abort"), sym("copy"), sym("type_name")],
            sym("String") => vec![sym("abort"), sym("copy"), sym("type_name"), sym("concat"), sym("length"), sym("substr")],
            sym("Apple") => vec![sym("abort"), sym("copy"), sym("type_name"), sym("foo")],
            sym("Orange") => vec![sym("abort"), sym("copy"), sym("type_name"), sym("foo"), sym("bar")],
        );

        assert_eq!(result.program_classes, desired_program_classes);
        assert_eq!(result.native_classes, desired_native_classes);
        assert_eq!(result.class_parent, desired_class_parent);
        assert_eq!(result.class_children, desired_class_children);
        assert_eq!(result.class_methods, desired_class_methods);
        assert_eq!(result.class_attrs, desired_class_attrs);
        assert_eq!(result.class_vtable, desired_class_vtable);

        assert_eq!(result.class_method_order, desired_class_method_order);
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
