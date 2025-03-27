mod codegen_class_inits;
mod codegen_constants;
mod codegen_expr;
mod codegen_functions;
mod codegen_globals;
mod codegen_native_methods;
mod codegen_utils;

use crate::ast::Program;
use crate::class_table::ClassTable;
use crate::env::Env;
use crate::symbol::Sym;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{IntType, PointerType, StructType};
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

pub struct CodeGenManager<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub variables: Env<PointerValue<'ctx>>,
    pub aspace: AddressSpace,
    pub ct: ClassTable,
    pub current_class: Option<Sym>,
    pub current_fn: Option<Sym>,
    pub i8_ty: IntType<'ctx>,
    pub i32_ty: IntType<'ctx>,
    pub bool_ty: IntType<'ctx>,
    pub ptr_ty: PointerType<'ctx>,
    pub cl_object_ty: StructType<'ctx>,
    pub cl_io_ty: StructType<'ctx>,
    pub cl_string_ty: StructType<'ctx>,
    pub cl_int_ty: StructType<'ctx>,
    pub cl_bool_ty: StructType<'ctx>,
}

impl<'ctx> CodeGenManager<'ctx> {
    pub fn from(context: &'ctx Context, program: &Program) -> Self {
        if !program.is_analyzed {
            panic!(
                "Cannot do codegen on un-analyzed program.  Run semantic analysis and try again."
            );
        }

        let builder = context.create_builder();
        let module = context.create_module("cool_module");
        let variables = Env::<PointerValue>::new();
        let aspace = AddressSpace::default();

        let ct = ClassTable::new(&program.classes).expect(
            "Failure to construct ClassTable \
            from program (should have been caught \
            in semantic analysis).",
        );
        let current_class = None;
        let current_fn = None;
        let i8_ty = context.i8_type();
        let i32_ty = context.i32_type();
        let bool_ty = context.bool_type();
        let ptr_ty = context.ptr_type(aspace);

        let cl_object_ty = context.opaque_struct_type("Object");
        let cl_io_ty = context.opaque_struct_type("IO");
        let cl_string_ty = context.opaque_struct_type("String");
        let cl_int_ty = context.opaque_struct_type("Int");
        let cl_bool_ty = context.opaque_struct_type("Bool");

        // Struct Types for Native Classes

        // class_id
        let object_attrs = &[i32_ty.into()];
        cl_object_ty.set_body(object_attrs, false);

        // class_id
        let io_attrs = &[i32_ty.into()];
        cl_io_ty.set_body(io_attrs, false);

        // class_id, value
        let int_attrs = &[i32_ty.into(), i32_ty.into()];
        cl_int_ty.set_body(int_attrs, false);

        // class_id, value
        let bool_attrs = &[i32_ty.into(), bool_ty.into()];
        cl_bool_ty.set_body(bool_attrs, false);

        // class_id, ptr to in for length, str content
        let string_attrs = &[
            i32_ty.into(),
            ptr_ty.into(),
            context.i8_type().array_type(0).into(),
        ];
        cl_string_ty.set_body(string_attrs, false);

        let mut man = CodeGenManager {
            context,
            builder,
            module,
            variables,
            aspace,
            ct,
            current_class,
            current_fn,
            i8_ty,
            i32_ty,
            bool_ty,
            ptr_ty,
            cl_object_ty,
            cl_io_ty,
            cl_string_ty,
            cl_int_ty,
            cl_bool_ty,
        };

        man.code_program_class_structs();

        man.code_all_method_declarations();

        man.code_vtable_master_vector();

        man.code_parent_vector();

        man.register_globals();

        man.code_type_name_vector();

        man.code_all_inits();

        man.code_all_method_bodies();

        man.code_main();

        man
    }
}

impl Program {
    pub fn to_llvm(&self, out_file: &str) {
        let context = Context::create();
        let man = CodeGenManager::from(&context, self);
        //man.module.verify().unwrap();
        man.module.print_to_file(out_file).unwrap();
    }
}

// Not sure why, but clipply complains about `serial` being unused.
#[allow(unused_imports)]
use serial_test::serial;

#[cfg(test)]
#[serial]
mod codegen_tests {

    use super::*;

    use assert_cmd::prelude::*;
    use predicates::prelude::*;
    use std::process::Command;
    use std::{env, fs};

    fn compile_run_assert_output_eq(code: &str, output: &str) {
        let mut program = Program::parse_from(code).unwrap();
        program.semant().unwrap();
        program.to_llvm("test_tmp.ll");
        run_assert_output_eq("test_tmp.ll", output);
    }

    fn run_assert_output_eq(filename: &str, output: &str) {
        let llvm_dir = env::var("LLVM_PATH").unwrap();
        let llc_path = format!("{}/bin/llc", llvm_dir);
        let clang_path = format!("{}/bin/clang", llvm_dir);
        let crate_root = std::env::current_dir().unwrap().display().to_string();
        let binary_path = format!("{}/test_tmp", crate_root);

        Command::new(llc_path).arg(filename).assert().success();
        Command::new(clang_path.clone())
            .arg("test_tmp.s")
            .arg("-c")
            .assert()
            .success();
        Command::new(clang_path)
            .arg("test_tmp.o")
            .arg("-o")
            .arg("test_tmp")
            .assert()
            .success();
        Command::new(binary_path.clone())
            .assert()
            .stdout(predicate::str::contains(output));

        fs::remove_file("test_tmp").unwrap();
        fs::remove_file("test_tmp.o").unwrap();
        fs::remove_file("test_tmp.s").unwrap();
        fs::remove_file("test_tmp.ll").unwrap();
    }

    #[test]
    fn test_codegen_simplest() {
        let code = "class Main{main():Object{0};};";
        compile_run_assert_output_eq(code, "");
    }

    #[test]
    fn test_codegen_simple_out_string() {
        let code = r#"class Main{main():Object{(new IO).out_string("hello")};};"#;
        compile_run_assert_output_eq(code, "hello");
    }

    #[test]
    fn test_codegen_simple_out_int() {
        let code = r#"class Main{main():Object{(new IO).out_int(42)};};"#;
        compile_run_assert_output_eq(code, "42");
    }

    #[test]
    fn test_codegen_object_abort() {
        let code = r#"
        class Apple {}; 
        class Main {
            main() : 
            Object {
                (new Apple).abort()
            };  
        };
        "#;
        compile_run_assert_output_eq(code, "");
    }

    // #[test]
    fn test_codegen_string_concat() {
        let code = r#"
        
       class Main {
    io: IO <- new IO; 
  main() : Object {{
      io.out_string("hello".concat(" world"));
    }}; 
};


        "#;
        compile_run_assert_output_eq(code, "hello world");
    }

    #[test]
    fn test_codegen_assign_attr() {
        let code = r#"
class Main {
    a: String <- "hello";
    io: IO <- (new IO);
    main() : Object {{
        io.out_string(a);
        a <- "bonjour";
        io.out_string(a);
        a <- "hola";
        io.out_string(a);
    }};  
};
"#;
        compile_run_assert_output_eq(code, "hello\nbonjour\nhola");
    }

    #[test]
    fn test_codegen_assign_local() {
        let code = r#"
class Main {
    a: String <- "hello";
    main() : Object {{
        (new IO).out_string(a);
        (new IO).out_string(foo(a));
    }};
    
    foo(a: String) : String {{
        a <- "goodbye";
        a;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "hello\ngoodbye");
    }

    //#[test]
    // How to do this?
    fn test_codegen_simple_in_string() {
        todo!();
    }

    //#[test]
    // How to do this?
    fn test_codegen_simple_in_int() {
        todo!();
    }

    #[test]
    fn test_codegen_string_length() {
        let code = r#"
class Main {
    io: IO <- (new IO);
    main() : Object {{
          io.out_int("hello".length());
          io.out_int("hell".length());
    }};  
};
"#;
        compile_run_assert_output_eq(code, "5\n4");
    }

    // #[test]
    fn test_codegen_string_substr() {
        let code = r#"
class Main {
    io: IO <- new IO; 
  main() : Object {{
      io.out_string("hello world".substr(2,3));
    }}; 
};

"#;
        compile_run_assert_output_eq(code, "llo");
    }

    #[test]
    fn test_codegen_dynamic_disp() {
        let code = r#"
    class Apple {
    greet() : Object {
    (new IO).out_string("Hello World!")};
};

class Orange inherits Apple {
    greet() : Object {(new IO).out_string("Hola Mundo!")};
};

class Main {
    a: Apple <- new Apple;
    b: Apple <- new Orange;
    main() : 
    Object {{
        a.greet();
        b.greet();
    }}; 
};
"#;
        compile_run_assert_output_eq(code, "Hello World!\nHola Mundo!");
    }

    #[test]
    fn test_codegen_object_type_name() {
        let code = r#"
    class Apple {};

    class Orange inherits Apple {};

    class Main {
        a: Apple <- new Apple;
        b: Apple <- new Orange;
        c: String;
        main() : 
        Object {{
            (new IO).out_string(a.type_name());
            (new IO).out_string(b.type_name());
            (new IO).out_string(c.type_name());
        }}; 
    };
    "#;
        compile_run_assert_output_eq(code, "Apple\nOrange\nString");
    }

    #[test]
    fn test_codegen_let() {
        let code = r#"
class Main {
        a: String <- "hello";
    main() : Object {{
        let a: String <- "goodbye" in (new IO).out_string(a);
        (new IO).out_string(a);
    }};  
};
"#;
        compile_run_assert_output_eq(code, "goodbye\nhello");
    }

    #[test]
    fn test_codegen_let_nested() {
        let code = r#"
class Main {
    main() : Object {
        let a: Int <- 2, b: Int <- 3 in (new IO).out_int(a + b)
    };
};
"#;
        compile_run_assert_output_eq(code, "5");
    }

    #[test]
    fn test_codegen_let_shadow_param() {
        let code = r#"
class Main {

    foo(a: Int) : Object {{
        let a: Int <- 2 in (new IO).out_int(a);
        (new IO).out_int(a);
    }};
    main() : Object {
        foo(1) 
    };
};
"#;
        compile_run_assert_output_eq(code, "2\n1");
    }

    #[test]
    fn test_codegen_cond() {
        let code = r#"
class Main {
    main() : Object {{
      if false then (new IO).out_string("YES") else (new IO).out_string("NO") fi;
      if true then (new IO).out_string("YES") else (new IO).out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "NO\nYES");
    }

    #[test]
    fn test_codegen_loop() {
        let code = r#"
class Main {
    a: Int <- 0;
    main() : Object {{
        (new IO).out_string("Time keeps on");
        while a < 3 loop {
            (new IO).out_string("slippin");
            a <- a + 1;
        } pool;
        (new IO).out_string("into the fuuuture....");
    }};  
};
"#;
        compile_run_assert_output_eq(
            code,
            "Time keeps on\nslippin\nslippin\nslippin\ninto the fuuuture....",
        );
    }

    #[test]
    fn test_codegen_function_read_param_return_value() {
        let code = r#"
class Main {
    a: Int <- 42; 
    f(x: Int) : Int {x + 1};
    main() : Object {
        (new IO).out_int(f(a))
    };  
};

"#;

        compile_run_assert_output_eq(code, "43");
    }

    #[test]
    fn test_codegen_variable_scoping() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    x: String <- "Zero";

    bar(x: String) : Object {{
        io.out_string(x);
    }};

    foo(x : String) : Object {{
        io.out_string(x);
        bar("Two");
        io.out_string(x);
    }};

    main() : Object {{
        io.out_string(x);
        foo("One");
        io.out_string(x);
    }};  
};

"#;
        compile_run_assert_output_eq(code, "Zero\nOne\nTwo\nOne\nZero");
    }

    #[test]
    fn test_codegen_int_equality() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {{
      if 42 = 42 then io.out_string("YES") else io.out_string("NO") fi;
      if 42 = 43 then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;

        compile_run_assert_output_eq(code, "YES\nNO");
    }

    #[test]
    fn test_codegen_string_equality() {
        let code = r#"
        class Main {
    io: IO <- new IO; 
    main() : Object {{
          if "hello" = "hello" then io.out_string("YES") else io.out_string("NO") fi;
          if "hello" = "bye" then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "YES\nNO");
    }

    #[test]
    fn test_codegen_pointer_equality() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    io1: IO <- new IO; 
    io2: IO <- new IO; 
    main() : Object {{
          if io1 = io2 then io.out_string("YES") else io.out_string("NO") fi;
          if io2 = io2 then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;

        compile_run_assert_output_eq(code, "NO\nYES");
    }

    #[test]
    fn test_codegen_lt() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {{
          if 42 < 42 then io.out_string("YES") else io.out_string("NO") fi;
          if 42 < 41 then io.out_string("YES") else io.out_string("NO") fi;
          if 41 < 42 then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;

        compile_run_assert_output_eq(code, "NO\nNO\nYES");
    }

    #[test]
    fn test_codegen_lte() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {{
          if 42 <= 42 then io.out_string("YES") else io.out_string("NO") fi;
          if 42 <= 41 then io.out_string("YES") else io.out_string("NO") fi;
          if 41 <= 42 then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "YES\nNO\nYES");
    }

    #[test]
    fn test_codegen_integer_comp() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {{
          io.out_int(~42 + 1);
          io.out_int(~0 + 1);
    }};  
};
"#;
        compile_run_assert_output_eq(code, "-42\n0");
    }

    #[test]
    fn test_codegen_logical_not() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {{
          if not true then io.out_string("YES") else io.out_string("NO") fi;
          if not false then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "NO\nYES");
    }

    #[test]
    fn test_codegen_isvoid() {
        let code = r#"
class Orange{};

class Main {
    io: IO <- new IO; 
    a: String;
    b: String <- "hello";
    c: Orange;
    main() : Object {{
          if isvoid(a) then io.out_string("YES") else io.out_string("NO") fi;
          if isvoid(b) then io.out_string("YES") else io.out_string("NO") fi;
          if isvoid(c) then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "NO\nNO\nYES");
    }

    #[test]
    fn test_codegen_arith() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {{
          io.out_int(2 + 3);
          io.out_int(3 - 1);
          io.out_int(2 - 3);
          io.out_int(2 * 3);
          io.out_int(6 / 2);
          io.out_int(6 / (0 - 2));
          io.out_int(6 / 4);
    }};  
};
"#;
        compile_run_assert_output_eq(code, "5\n2\n-1\n6\n3\n-3\n1");
    }

    #[test]
    fn test_codegen_typecase() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {
        case 42 of 
        y : Object => io.out_string("Object");
        x : Int => io.out_int(x + 2);
        esac
    };
};
"#;
        compile_run_assert_output_eq(code, "44");
    }

    #[test]
    fn test_codegen_typecase_complex() {
        let code = r#"
        class A{};
        class A1 inherits A{};
        class A2 inherits A{};
        class A11 inherits A1{};
        class A12 inherits A1{};
        class A121 inherits A12{};
        class A122 inherits A12{};
        class A21 inherits A2{};
        class A22 inherits A2{};

class Main {
    io: IO <- new IO; 
    a : A <- new A122;
    main() : Object {
        case a of 
        a : A1 => io.out_string("A1.");
        a : A2 => io.out_string("A2.");
        a : A11 => io.out_string("A11.");
        a : A12 => io.out_string("A12.");
        a : A121 => io.out_string("A121.");
        (* a : A122 => io.out_string("A122."); (no case for A122) *)
        a : A21 => io.out_string("A21.");
        a : A22 => io.out_string("A22.");
        esac
    };
};
"#;
        compile_run_assert_output_eq(code, "A12.");
    }

    #[test]
    fn test_codegen_typecase_complex_2() {
        let code = r#"
        class A{};
        class A1 inherits A{};
        class A2 inherits A{};
        class A11 inherits A1{};
        class A12 inherits A1{};
        class A121 inherits A12{};
        class A122 inherits A12{};
        class A21 inherits A2{};
        class A22 inherits A2{};

class Main {
    io: IO <- new IO; 
    a : A <- new A122;
    main() : Object {
        case a of 
        a : A1 => io.out_string("A1.");
        a : A2 => io.out_string("A2.");
        a : A11 => io.out_string("A11.");
        (* a : A12 => io.out_string("A12."); (no case for A12) *)
        a : A121 => io.out_string("A121.");
        (* a : A122 => io.out_string("A122."); (no case for A122) *)
        a : A21 => io.out_string("A21.");
        a : A22 => io.out_string("A22.");
        esac
    };
};
"#;
        compile_run_assert_output_eq(code, "A1.");
    }
}
