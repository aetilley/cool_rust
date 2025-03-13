#![allow(clippy::self_named_constructors)]

use std::fmt::Debug;

use crate::symbol::{sym, Sym};

pub fn add_one<T: Clone>(some: &Vec<T>, one: &T) -> Vec<T> {
    // Takes ownership
    let mut res = some.to_owned();
    res.push(one.to_owned());
    res
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub classes: Classes,
}
impl Program {
    pub fn program(classes: Classes) -> Program {
        Program { classes }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: Sym,
    pub parent: Sym,
    pub features: Features,
}
pub type Classes = Vec<Class>;

impl Class {
    pub fn class(name: &str, parent: &str, features: Features) -> Class {
        Class {
            name: sym(name),
            parent: sym(parent),
            features,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Formal {
    pub name: Sym,
    pub typ: Sym,
}
pub type Formals = Vec<Formal>;
impl Formal {
    pub fn formal(name: &str, typ: &str) -> Formal {
        Formal {
            name: sym(name),
            typ: sym(typ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Feature {
    Attr {
        name: Sym,
        typ: Sym,
        init: Expr,
    },
    Method {
        name: Sym,
        formals: Formals,
        typ: Sym,
        body: Expr,
    },
}
pub type Features = Vec<Feature>;

impl Feature {
    pub fn attr(name: &str, typ: &str, init: Expr) -> Feature {
        Feature::Attr {
            name: sym(name),
            typ: sym(typ),
            init,
        }
    }
    pub fn method(name: &str, formals: Formals, typ: &str, body: Expr) -> Feature {
        Feature::Method {
            name: sym(name),
            formals,
            typ: sym(typ),
            body,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    pub id: Sym,
    pub typ: Sym,
    pub expr: Expr,
}
pub type Cases = Vec<Case>;
impl Case {
    pub fn case(id: &str, typ: &str, expr: Expr) -> Self {
        Case {
            id: sym(id),
            typ: sym(typ),
            expr,
        }
    }
}

// Intermediate datastructure.  Will not end up in
// any final AST.
#[derive(Clone, Debug, PartialEq)]
pub struct LetBinding {
    id: Sym,
    typ: Sym,
    init: Expr,
}
pub type LetBindings = Vec<LetBinding>;
impl LetBinding {
    pub fn let_binding(id: &str, typ: &str, init: Expr) -> LetBinding {
        LetBinding {
            id: sym(id),
            typ: sym(typ),
            init,
        }
    }

    pub fn unwrap_let_bindings(let_bindings: LetBindings, body: Expr) -> Expr {
        if let_bindings.is_empty() {
            return body;
        }
        let head = let_bindings[0].to_owned();
        let tail = let_bindings[1..].to_owned();
        let new_body = LetBinding::unwrap_let_bindings(tail, body);
        Expr::r#let(&head.id, &head.typ, head.init, new_body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprData {
    NoExpr {},
    Object {
        id: Sym,
    },
    BoolConst {
        val: bool,
    },
    IntConst {
        val: Sym,
    },
    StrConst {
        val: Sym,
    },
    Dispatch {
        slf: Expr,
        method_name: Sym,
        args: Exprs,
    },
    StaticDispatch {
        typ: Sym,
        method_name: Sym,
        args: Exprs,
        slf: Expr,
    },
    New {
        typ: Sym,
    },
    TypCase {
        expr: Expr,
        cases: Cases,
    },
    Let {
        id: Sym,
        typ: Sym,
        init: Expr,
        body: Expr,
    },
    Loop {
        pred: Expr,
        body: Expr,
    },
    Block {
        exprs: Exprs,
    },
    Cond {
        pred: Expr,
        then_expr: Expr,
        else_expr: Expr,
    },
    Assign {
        id: Sym,
        expr: Expr,
    },
    Not {
        expr: Expr,
    },
    IsVoid {
        expr: Expr,
    },
    Comp {
        expr: Expr,
    },
    Eq {
        lhs: Expr,
        rhs: Expr,
    },
    Leq {
        lhs: Expr,
        rhs: Expr,
    },
    Lt {
        lhs: Expr,
        rhs: Expr,
    },
    Plus {
        lhs: Expr,
        rhs: Expr,
    },
    Minus {
        lhs: Expr,
        rhs: Expr,
    },
    Times {
        lhs: Expr,
        rhs: Expr,
    },
    Divide {
        lhs: Expr,
        rhs: Expr,
    },
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub data: Box<ExprData>,
    // The static type, to be determined during semantic analysis.
    pub stype: Sym,
    // The byte range of the Expr in the source code.
    pub span: (usize, usize),
}
pub type Exprs = Vec<Expr>;
impl Expr {
    pub fn from(data: ExprData) -> Self {
        let stype = sym("No_type");
        let span = (0, 0);
        Expr {
            data: Box::new(data),
            stype,
            span,
        }
    }
}
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        // We ignore ignore metadata to simplify testing.
        // (It's rare that we'd be checking two ASTs for identity
        // outside of testing.)
        self.data == other.data
    }
}

impl Expr {
    pub fn r#let(id: &str, typ: &str, init: Expr, body: Expr) -> Expr {
        Expr::from(ExprData::r#let(id, typ, init, body))
    }

    pub fn no_expr() -> Expr {
        Expr::from(ExprData::no_expr())
    }

    pub fn object(id: &str) -> Expr {
        Expr::from(ExprData::object(id))
    }

    pub fn bool_const(val: bool) -> Expr {
        Expr::from(ExprData::bool_const(val))
    }

    pub fn int_const(val: &str) -> Expr {
        Expr::from(ExprData::int_const(val))
    }

    pub fn str_const(val: &str) -> Expr {
        Expr::from(ExprData::str_const(val))
    }

    pub fn dispatch(slf: Expr, method_name: &str, args: Exprs) -> Expr {
        Expr::from(ExprData::dispatch(slf, method_name, args))
    }

    pub fn static_dispatch(slf: Expr, typ: &str, method_name: &str, args: Exprs) -> Expr {
        Expr::from(ExprData::static_dispatch(slf, typ, method_name, args))
    }

    pub fn new(typ: &str) -> Expr {
        Expr::from(ExprData::new(typ))
    }

    pub fn typcase(expr: Expr, cases: Cases) -> Expr {
        Expr::from(ExprData::typcase(expr, cases))
    }
    pub fn r#loop(pred: Expr, body: Expr) -> Expr {
        Expr::from(ExprData::r#loop(pred, body))
    }

    pub fn cond(pred: Expr, then_expr: Expr, else_expr: Expr) -> Expr {
        Expr::from(ExprData::cond(pred, then_expr, else_expr))
    }

    pub fn block(exprs: Exprs) -> Expr {
        Expr::from(ExprData::block(exprs))
    }

    pub fn assign(id: &str, expr: Expr) -> Expr {
        Expr::from(ExprData::assign(id, expr))
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(expr: Expr) -> Expr {
        Expr::from(ExprData::not(expr))
    }

    pub fn isvoid(expr: Expr) -> Expr {
        Expr::from(ExprData::isvoid(expr))
    }

    pub fn comp(expr: Expr) -> Expr {
        Expr::from(ExprData::comp(expr))
    }

    pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::lt(lhs, rhs))
    }

    pub fn leq(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::leq(lhs, rhs))
    }

    pub fn eq(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::eq(lhs, rhs))
    }

    pub fn plus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::plus(lhs, rhs))
    }

    pub fn minus(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::minus(lhs, rhs))
    }

    pub fn times(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::times(lhs, rhs))
    }

    pub fn divide(lhs: Expr, rhs: Expr) -> Expr {
        Expr::from(ExprData::divide(lhs, rhs))
    }
}

impl ExprData {
    pub fn r#let(id: &str, typ: &str, init: Expr, body: Expr) -> ExprData {
        ExprData::Let {
            id: sym(id),
            typ: sym(typ),
            init,
            body,
        }
    }

    pub fn no_expr() -> ExprData {
        ExprData::NoExpr {}
    }

    pub fn object(id: &str) -> ExprData {
        ExprData::Object { id: sym(id) }
    }

    pub fn bool_const(val: bool) -> ExprData {
        ExprData::BoolConst { val }
    }

    pub fn int_const(val: &str) -> ExprData {
        ExprData::IntConst { val: sym(val) }
    }

    pub fn str_const(val: &str) -> ExprData {
        ExprData::StrConst { val: sym(val) }
    }

    pub fn dispatch(slf: Expr, method_name: &str, args: Exprs) -> ExprData {
        ExprData::Dispatch {
            slf,
            method_name: sym(method_name),
            args,
        }
    }

    pub fn static_dispatch(elem: Expr, typ: &str, method_name: &str, args: Exprs) -> ExprData {
        ExprData::StaticDispatch {
            slf: elem,
            typ: sym(typ),
            method_name: sym(method_name),
            args,
        }
    }

    pub fn new(typ: &str) -> ExprData {
        ExprData::New { typ: sym(typ) }
    }

    pub fn typcase(expr: Expr, cases: Cases) -> ExprData {
        ExprData::TypCase { expr, cases }
    }
    pub fn r#loop(pred: Expr, body: Expr) -> ExprData {
        ExprData::Loop { pred, body }
    }

    pub fn cond(pred: Expr, then_expr: Expr, else_expr: Expr) -> ExprData {
        ExprData::Cond {
            pred,
            then_expr,
            else_expr,
        }
    }

    pub fn block(exprs: Exprs) -> ExprData {
        ExprData::Block { exprs }
    }

    pub fn assign(id: &str, expr: Expr) -> ExprData {
        ExprData::Assign { id: sym(id), expr }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn not(expr: Expr) -> ExprData {
        ExprData::Not { expr }
    }

    pub fn isvoid(expr: Expr) -> ExprData {
        ExprData::IsVoid { expr }
    }

    pub fn comp(expr: Expr) -> ExprData {
        ExprData::Comp { expr }
    }

    pub fn lt(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Lt { lhs, rhs }
    }

    pub fn leq(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Leq { lhs, rhs }
    }

    pub fn eq(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Eq { lhs, rhs }
    }

    pub fn plus(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Plus { lhs, rhs }
    }

    pub fn minus(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Minus { lhs, rhs }
    }

    pub fn times(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Times { lhs, rhs }
    }

    pub fn divide(lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Divide { lhs, rhs }
    }
}
