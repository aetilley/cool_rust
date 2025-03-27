use crate::ast::{Expr, ExprData, Formal};
use crate::symbol::{sym, Sym};
use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicMetadataTypeEnum, FunctionType};
use inkwell::values::{FunctionValue, IntValue, PointerValue};

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

// Code function stubs.
// We separate this because function body coding must come last, and meanwhile things like
// globals will require function declarations.
impl<'ctx> CodeGenManager<'ctx> {
    pub fn get_function_type_from_signature(
        &self,
        parameters: &[Formal],
        _return_type: &Sym,
    ) -> FunctionType<'ctx> {
        // Everything is a pointer.
        let args_types = std::iter::repeat(self.ptr_ty)
            .take(parameters.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = self.ptr_ty.fn_type(&args_types[..], false);
        fn_type
    }

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
impl<'ctx> CodeGenManager<'ctx> {
    pub fn code_function_entry(&self, fn_name: &str) -> (FunctionValue<'ctx>, BasicBlock<'ctx>) {
        // For predeclared functions only!
        let fn_val = self
            .module
            .get_function(fn_name)
            .unwrap_or_else(|| panic!("No function declaration found for {}", fn_name));
        let block_name = &format!("{}_entry", fn_name);
        let entry_block = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry_block);
        (fn_val, entry_block)
    }

    // Class Initialization function bodies
    fn make_code_init_body_wrapper(
        &mut self,
        name: &Sym,
        code_body: fn(&mut CodeGenManager<'ctx>, &str, PointerValue),
    ) {
        let class_id = self.sym_to_class_id_int_val(name);

        // Takes care of boilerplate function setup and calls `code_body`.
        let fn_name = init_ref(name);
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let first = fn_val.get_first_param().unwrap();
        let self_ptr = first.into_pointer_value();

        // Class id
        let pointee_ty = self.context.get_struct_type(name).unwrap();
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_ptr, CLASS_ID_IND, "gep")
            .unwrap();
        self.builder.build_store(field, class_id).unwrap();

        // Code Body
        code_body(self, name, self_ptr);
        let null = self.context.ptr_type(self.aspace).const_null();
        self.builder.build_return(Some(&null)).unwrap();
        fn_val.verify(false);
    }

    fn code_empty_init_body(&mut self, _name: &str, _self_alloca: PointerValue) {}

    fn code_int_init_body(&mut self, _name: &str, self_alloca: PointerValue) {
        // set val = 0
        let value = self.i32_ty.const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(self.cl_int_ty, self_alloca, INT_VAL_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_bool_init_body(&mut self, _name: &str, self_alloca: PointerValue) {
        // set val = false
        let value = self.bool_ty.const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(self.cl_bool_ty, self_alloca, BOOL_VAL_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_string_init_body(&mut self, _name: &str, self_alloca: PointerValue) {
        // set Length = 0
        let value = self.i32_ty.const_int(0, false);
        let value_ptr = self.code_new_int(value);
        let field = self
            .builder
            .build_struct_gep(self.cl_string_ty, self_alloca, STRING_LEN_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value_ptr).unwrap();

        // set ptr -> ""

        let value = self.i8_ty.const_array(&[]);
        let field = self
            .builder
            .build_struct_gep(self.cl_string_ty, self_alloca, STRING_CONTENT_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    pub fn sym_to_class_id_int_val(&self, s: &Sym) -> IntValue<'ctx> {
        self.i32_ty.const_int(
            self.ct
                .class_id
                .get(s)
                .unwrap_or_else(|| panic!("No class id found for {}", s))
                .to_owned()
                .try_into()
                .unwrap(),
            false,
        )
    }

    fn code_native_inits(&mut self) {
        self.make_code_init_body_wrapper(&sym("Object"), CodeGenManager::code_empty_init_body);
        self.make_code_init_body_wrapper(&sym("IO"), CodeGenManager::code_empty_init_body);
        self.make_code_init_body_wrapper(&sym("Int"), CodeGenManager::code_int_init_body);
        self.make_code_init_body_wrapper(&sym("Bool"), CodeGenManager::code_bool_init_body);
        self.make_code_init_body_wrapper(&sym("String"), CodeGenManager::code_string_init_body);
    }

    fn code_init_body_for_class(&mut self, class_name: &str, self_alloca: PointerValue) {
        let attrs = self.ct.class_attrs.get(&sym(class_name)).unwrap().clone();

        for (ind, (_, typ, init)) in attrs.iter().enumerate() {
            let value = if (*init.data == ExprData::NoExpr {}) {
                let key: &str = typ;
                if ["Int", "Bool", "String"].contains(&key) {
                    self.code_new_and_init(typ)
                } else {
                    self.ptr_ty.const_null()
                }
            } else {
                self.codegen(init)
            };
            let cast_ind: u32 = ind.try_into().unwrap();
            let field_index: u32 = cast_ind + OBJECT_PREFIX_SIZE;
            let pointee_ty = self.context.get_struct_type(class_name).unwrap();
            let field = self
                .builder
                .build_struct_gep(pointee_ty, self_alloca, field_index, "gep")
                .unwrap();
            self.builder.build_store(field, value).unwrap();
        }
    }

    fn code_init_for_class(&mut self, name: Sym) {
        self.make_code_init_body_wrapper(&name, CodeGenManager::code_init_body_for_class);
    }

    pub fn code_all_inits(&mut self) {
        self.code_native_inits();

        let classes = self.ct.program_classes.clone();
        for cls in classes.iter() {
            // Register class id for program class
            self.code_init_for_class(cls.to_owned());
        }
    }
}

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
