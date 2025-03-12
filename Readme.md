WIP!

COOL Language manual:  https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

Status:  Semantic analysis almost complete.

Next up:  LLVM backend

Warts:  Currently errors reported do not come with a line or byte number.  We can attach the bytes to the Ast during parsing, but would prefer not to include this much data in test cases.  Will revisit soon.


Usage:  

```
$ cargo run -- simple.cl 
Program {
    classes: [
        Class {
            name: u!("Apple"),
            parent: u!("Object"),
            features: [
                Attr {
                    name: u!("b"),
                    typ: u!("Int"),
                    init: Expr {
                        data: IntConst {
                            val: u!("42"),
                        },
                        stype: u!("Int"),
                    },
                },
            ],
        },
        Class {
            name: u!("Orange"),
            parent: u!("Apple"),
            features: [
                Method {
                    name: u!("foo"),
                    formals: [],
                    typ: u!("Str"),
                    body: Expr {
                        data: New {
                            typ: u!("Str"),
                        },
                        stype: u!("Str"),
                    },
                },
            ],
        },
        Class {
            name: u!("Banana"),
            parent: u!("Object"),
            features: [
                Attr {
                    name: u!("carol"),
                    typ: u!("Apple"),
                    init: Expr {
                        data: NoExpr,
                        stype: u!("No_type"),
                    },
                },
                Method {
                    name: u!("bar"),
                    formals: [
                        Formal {
                            name: u!("x"),
                            typ: u!("Orange"),
                        },
                    ],
                    typ: u!("Apple"),
                    body: Expr {
                        data: Cond {
                            pred: Expr {
                                data: BoolConst {
                                    val: true,
                                },
                                stype: u!("Bool"),
                            },
                            then_expr: Expr {
                                data: Object {
                                    id: u!("carol"),
                                },
                                stype: u!("Apple"),
                            },
                            else_expr: Expr {
                                data: Object {
                                    id: u!("x"),
                                },
                                stype: u!("Orange"),
                            },
                        },
                        stype: u!("Apple"),
                    },
                },
            ],
        },
    ],
}
```
