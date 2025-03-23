pub mod codegen_constants;
pub mod codegen_expr;
pub mod codegen_functions;
pub mod codegen_globals;
pub mod codegen_structs;
pub mod codegen_utils;

use crate::ast::Program;
use crate::class_table::ClassTable;
use crate::env::Env;
use crate::symbol::Sym;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
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
}

impl<'ctx> CodeGenManager<'ctx> {
    pub fn from(context: &'ctx Context, program: &Program) -> Self {
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

        let mut man = CodeGenManager {
            context,
            builder,
            module,
            variables,
            aspace,
            ct,
            current_class,
            current_fn,
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

#[cfg(test)]
mod codegen_tests {

    use super::*;

    use crate::ast_parse::Parse;

    #[test]
    fn test_codegen_simplest() {
        let context = Context::create();
        let mut program = Program::parse("class Main{main():Object{0};};").unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
    }

    #[test]
    fn test_codegen_bool_1() {
        let context = Context::create();
        let mut program = Program::parse("class Main{main():Object{true};};").unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
    }

    #[test]
    fn test_codegen_dynamic_disp() {
        let code = r#"
    class Apple {
    greet() : Object {
    (new IO).out_string("Hello World!")};
};

class Orange inherits Apple {
    greet() : Object {(new IO).out_string("Hola, Mundo!")};
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
        let context = Context::create();
        let mut program = Program::parse(code).unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
    }

    #[test]
    fn test_codegen_cond() {
        let code = r#"
class Main {
    main() : 
    Object {
      if false then 42 else 43 fi
    };  
};
"#;
        let context = Context::create();
        let mut program = Program::parse(code).unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
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
        let context = Context::create();
        let mut program = Program::parse(code).unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
    }
    #[test]
    fn test_codegen_int_equality() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    main() : Object {
      if 42 = 42 then io.out_string("YES") else io.out_string("NO") fi
    };  
};
"#;
        let context = Context::create();
        let mut program = Program::parse(code).unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
    }

    #[test]
    fn test_codegen_function_string_equality() {
        let code = r#"
        class Main {
    io: IO <- new IO; 
    main() : Object {
          if "hello" = "bye" then io.out_string("YES") else io.out_string("NO") fi
    };  
};
"#;
        let context = Context::create();
        let mut program = Program::parse(code).unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
    }

    #[test]
    fn test_codegen_function_pointer_equality() {
        let code = r#"
class Main {
    io: IO <- new IO; 
    io1: IO <- new IO; 
    io2: IO <- new IO; 
    main() : Object {
          if io2 = io2 then io.out_string("YES") else io.out_string("NO") fi
    };  
};
"#;
        let context = Context::create();
        let mut program = Program::parse(code).unwrap();
        program.semant().unwrap();
        let man = CodeGenManager::from(&context, &program);
        man.module.verify().unwrap();
    }
}
