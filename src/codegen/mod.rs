mod codegen_constants;
mod codegen_expr;
mod codegen_functions;
mod codegen_globals;
mod codegen_structs;
mod codegen_utils;

use crate::ast::Program;
use crate::class_table::ClassTable;
use crate::env::Env;
use crate::symbol::Sym;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{IntType, PointerType};
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
    pub int32ty: IntType<'ctx>,
    pub boolty: IntType<'ctx>,
    pub ptrty: PointerType<'ctx>,
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
        let int32ty = context.i32_type();
        let boolty = context.bool_type();
        let ptrty = context.ptr_type(aspace);

        let mut man = CodeGenManager {
            context,
            builder,
            module,
            variables,
            aspace,
            ct,
            current_class,
            current_fn,
            int32ty,
            boolty,
            ptrty,
        };

        man.code_all_class_structs();

        man.code_all_method_declarations();

        man.code_vtables();

        man.register_globals();

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
        man.module.verify().unwrap();
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

    use crate::ast::parse::Parse;

    use assert_cmd::prelude::*;
    use predicates::prelude::*;
    use std::process::Command;
    use std::{env, fs};

    fn compile_run_assert_output_eq(code: &str, output: &str) {
        let llvm_dir = env::var("LLVM_PATH").unwrap();
        let llc_path = format!("{}/bin/llc", llvm_dir);
        let clang_path = format!("{}/bin/clang", llvm_dir);
        let crate_root = std::env::current_dir().unwrap().display().to_string();
        let binary_path = format!("{}/test_tmp", crate_root);

        let mut program = Program::parse(code).unwrap();
        program.semant().unwrap();
        program.to_llvm("test_tmp.ll");

        Command::new(llc_path).arg("test_tmp.ll").assert().success();
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

    #[test]
    fn test_codegen_simple_out_string() {
        let code = r#"class Main{main():Object{(new IO).out_string("hello")};};"#;
        compile_run_assert_output_eq(code, "hello");
    }

    //#[test]
    // How to do this?
    fn test_codegen_simple_in_string() {
        todo!();
    }

    #[test]
    fn test_codegen_string_length() {
        let code = r#"
class Main {
    io: IO <- (new IO);
    main() : Object {{
          if "hello".length() = 5 then io.out_string("YES") else io.out_string("NO") fi;
          if "hell".length() = 5 then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "YES\nNO");
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
    fn test_codegen_cond() {
        let code = r#"
class Main {
    main() : 
    Object {{
      if false then (new IO).out_string("YES") else (new IO).out_string("NO") fi;
      if true then (new IO).out_string("YES") else (new IO).out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "NO\nYES");
    }

    #[test]
    fn test_codegen_function_read_param_return_value() {
        let code = r#"
class Main {
    a: Int <- 42; 
    io: IO <- new IO; 
    f(x: Int) : Bool {x = 42};
    main() : Object {
      if f(a) then io.out_string("YES") else io.out_string("NO") fi
    };  
};

"#;

        compile_run_assert_output_eq(code, "YES");
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
          if ~42 + 1 = 0-42 then io.out_string("YES") else io.out_string("NO") fi;
          if ~0 + 1 = 0 then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "YES\nYES");
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
          if  2 + 3 = 5 then io.out_string("YES") else io.out_string("NO") fi;
          if  3 - 1 = 2 then io.out_string("YES") else io.out_string("NO") fi;
          if  2 - 3 = 0 - 1 then io.out_string("YES") else io.out_string("NO") fi;
          if  2 * 3 = 6 then io.out_string("YES") else io.out_string("NO") fi;
          if  6 / 2 = 3 then io.out_string("YES") else io.out_string("NO") fi;
          if  6 / (0 - 2) = (0 - 3) then io.out_string("YES") else io.out_string("NO") fi;
          if  6 / 4 = 1 then io.out_string("YES") else io.out_string("NO") fi;
    }};  
};
"#;
        compile_run_assert_output_eq(code, "YES\nYES\nYES\nYES\nYES\nYES\n");
    }
}
