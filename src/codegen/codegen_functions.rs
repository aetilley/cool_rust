use crate::ast::{Expr, Formal};
use crate::symbol::{sym, Sym};

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

// Code function stubs.
// We separate this because function body coding must come last, and meanwhile things like
// globals will require function declarations.
impl CodeGenManager<'_> {
    pub fn code_all_method_declarations(&mut self) {
        let classes = self.ct.program_classes.clone();
        for cls in classes {
            self.code_class_init_declaration(&cls);
            let methods = self.ct.class_methods.get(&cls).unwrap().clone();
            for (method, ((parameters, return_type), _)) in methods.iter() {
                self.code_method_declaration(method_ref(&cls, method), parameters, return_type);
            }
        }
        let native_classes = self.ct.native_classes.clone();
        for cls in native_classes {
            self.code_class_init_declaration(&cls);
            let methods = self.ct.class_methods.get(&cls).unwrap().clone();
            for (method, ((parameters, return_type), _)) in methods.iter() {
                self.code_method_declaration(method_ref(&cls, method), parameters, return_type);
            }
        }
    }

    fn code_method_declaration(
        &mut self,
        fn_name: String,
        parameters: &[Formal],
        return_type: &Sym,
    ) {
        // Must add self parameter.
        let mut all_parameters = vec![Formal::formal("self", "SELF_TYPE")];
        all_parameters.extend(parameters.to_owned());

        let fn_type = self.get_function_type_from_signature(&all_parameters, return_type);
        let fn_val = self.module.add_function(&fn_name, fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_pointer_value().set_name(&all_parameters[i].name);
        }
    }

    fn code_class_init_declaration(&mut self, cls: &Sym) {
        self.code_method_declaration(init_ref(cls), &[], &sym(OBJECT))
    }
}

// Function Bodies

// Method bodies
impl CodeGenManager<'_> {
    fn code_method_body(
        &mut self,
        cls: &Sym,
        method: &Sym,
        parameters: &[Formal],
        _return_type: &Sym,
        body: &Expr,
    ) {
        let fn_name = method_ref(cls, method);
        let fn_val = self.module.get_function(&fn_name).unwrap();

        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

        // Must add self parameter.
        let mut all_parameters = vec![Formal::formal("self", "SELF_TYPE")];
        all_parameters.extend(parameters.to_owned());
        // Build Env
        self.variables.enter_scope();
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let arg_name = all_parameters[i].name.clone();
            let alloca = self.builder.build_alloca(self.ptr_ty, &arg_name).unwrap();

            self.builder.build_store(alloca, arg).unwrap();

            self.variables.add_binding(&all_parameters[i].name, &alloca);
        }

        self.current_fn = Some(sym(&fn_name));
        let body_val = self.codegen(body);
        self.builder.build_return(Some(&body_val)).unwrap();
        self.variables.exit_scope();
        fn_val.verify(true);
    }

    fn code_method_bodies_for_class(&mut self, cls: &Sym) {
        let methods = self.ct.class_methods.get(cls).unwrap().clone();
        self.current_class = Some(cls.to_owned());
        for (method, ((parameters, return_type), body)) in methods.iter() {
            self.code_method_body(cls, method, parameters, return_type, body);
        }
        self.current_class = None;
    }

    fn code_program_method_bodies(&mut self) {
        let classes = self.ct.program_classes.clone();
        for cls in classes {
            self.code_method_bodies_for_class(&cls);
        }
    }

    pub fn code_all_method_bodies(&mut self) {
        self.code_native_method_bodies();
        self.code_program_method_bodies();
    }

    // main (Entrypoint)
    pub fn code_main(&mut self) {
        // The entrypoint for the program.  Clang needs this to be called main, but it will
        // 1) create an instance of the Main class and
        // 2) call *that* class's `main` method (ie. Main.main).

        let fn_type = self.context.void_type().fn_type(&[], false);
        let fn_name = "main";
        let fn_val = self.module.add_function(fn_name, fn_type, None);
        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

        let main_dot_main = self.module.get_function("Main.main").unwrap();
        let main_instance = self.code_new_and_init(&sym("Main"));
        self.builder
            .build_call(main_dot_main, &[main_instance.into()], "call_Main.main")
            .unwrap();
        self.builder.build_return(None).unwrap();
        fn_val.verify(false);
    }
}
