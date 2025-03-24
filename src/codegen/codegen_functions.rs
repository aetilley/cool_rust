use crate::ast::{Expr, ExprData, Formal};
use crate::symbol::{sym, Sym};
use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, FunctionType};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, PointerValue};

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

// Code function stubs.
impl<'ctx> CodeGenManager<'ctx> {
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

    pub fn get_function_type_from_signature(
        &self,
        parameters: &[Formal],
        _return_type: &Sym,
    ) -> FunctionType<'ctx> {
        // TODO! Currently ignoring the types of args / return and treating them as generic pointers.
        let ret_type = self.context.ptr_type(self.aspace);
        let args_types = std::iter::repeat(ret_type)
            .take(parameters.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = ret_type.fn_type(&args_types[..], false);
        fn_type
    }
}

// Function Bodies
impl<'ctx> CodeGenManager<'ctx> {
    // Class Initialization function bodies
    fn make_code_init_body_wrapper(
        &self,
        name: &Sym,
        code_body: fn(&CodeGenManager<'ctx>, &str, PointerValue),
    ) {
        // Takes care of boilerplate function setup and calls `code_body`.
        let fn_name = init_ref(name);

        let fn_val = self
            .module
            .get_function(&fn_name)
            .unwrap_or_else(|| panic!("No declaration found for {}", fn_name));

        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

        let first = fn_val.get_first_param().unwrap();
        let self_ptr = first.into_pointer_value();

        // Install Vtable
        let vtable_name = &format!("{}_vtable", name);
        let ptr = self
            .module
            .get_global(vtable_name)
            .unwrap()
            .as_pointer_value();

        let pointee_ty = self.context.get_struct_type(name).unwrap();
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_ptr, VTABLE_IND, "gep")
            .unwrap();
        self.builder.build_store(field, ptr).unwrap();

        // Code Body
        code_body(self, name, self_ptr);
        let null = self.context.ptr_type(self.aspace).const_null();
        self.builder.build_return(Some(&null)).unwrap();
        fn_val.verify(false);
    }

    fn code_empty_init_body(&self, _name: &str, _self_alloca: PointerValue) {}

    fn code_int_init_body(&self, _name: &str, self_alloca: PointerValue) {
        let pointee_ty = self.context.get_struct_type("Int").unwrap();
        // set val = 0
        let value = self.context.i32_type().const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, INT_VAL_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_bool_init_body(&self, _name: &str, self_alloca: PointerValue) {
        let pointee_ty = self.context.get_struct_type("Bool").unwrap();
        // set val = false
        let value = self.context.bool_type().const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, BOOL_VAL_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_string_init_body(&self, _name: &str, self_alloca: PointerValue) {
        let pointee_ty = self.context.get_struct_type("String").unwrap();
        // set Length = 0
        let value = self.context.i32_type().const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, STRING_LEN_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();

        // set ptr -> ""

        let value = self.context.i8_type().const_array(&[]);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, STRING_CONTENT_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_native_inits(&self) {
        self.make_code_init_body_wrapper(&sym("Object"), CodeGenManager::code_empty_init_body);
        self.make_code_init_body_wrapper(&sym("IO"), CodeGenManager::code_empty_init_body);
        self.make_code_init_body_wrapper(&sym("Int"), CodeGenManager::code_int_init_body);
        self.make_code_init_body_wrapper(&sym("Bool"), CodeGenManager::code_bool_init_body);
        self.make_code_init_body_wrapper(&sym("String"), CodeGenManager::code_string_init_body);
    }

    fn code_init_body_for_class(&self, class_name: &str, self_alloca: PointerValue) {
        let attrs = self
            .ct
            .class_attrs
            .get(&sym(class_name))
            .unwrap()
            .iter()
            .enumerate();
        for (ind, (_, typ, init)) in attrs {
            let value = if (*init.data == ExprData::NoExpr {}) {
                let key: &str = typ;
                if ["Int", "Bool", "String"].contains(&key) {
                    self.code_new_and_init(typ)
                } else {
                    self.context.ptr_type(self.aspace).const_null()
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

    fn code_init_for_class(&self, name: Sym) {
        self.make_code_init_body_wrapper(&name, CodeGenManager::code_init_body_for_class);
    }

    pub fn code_all_inits(&self) {
        self.code_native_inits();

        for class in self.ct.program_classes.iter() {
            self.code_init_for_class(class.to_owned());
        }
    }

    // Method bodies

    fn declare_puts(&self) {
        let input_type = BasicMetadataTypeEnum::PointerType(self.context.ptr_type(self.aspace));
        let puts_type = self.context.i32_type().fn_type(&[input_type], false);
        self.module
            .add_function("puts", puts_type, Some(Linkage::External));
    }

    fn declare_strcmp(&self) {
        let input_type = BasicMetadataTypeEnum::PointerType(self.context.ptr_type(self.aspace));
        let puts_type = self
            .context
            .i32_type()
            .fn_type(&[input_type, input_type], false);
        self.module
            .add_function("strcmp", puts_type, Some(Linkage::External));
    }

    fn declare_gets(&self) {
        //let array_type = self.context.i8_type().array_type(2);
        let typ = self.context.ptr_type(self.aspace);
        let fn_type = typ.fn_type(&[typ.into()], false);
        self.module
            .add_function("gets", fn_type, Some(Linkage::External));
    }

    fn code_function_entry(&self, fn_name: &str) -> (FunctionValue, BasicBlock) {
        // For predefined functions only!
        let fn_val = self.module.get_function(&fn_name).unwrap();
        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);
        return (fn_val, entry);
    }

    fn code_io_out_string(&self) {
        let fn_name = method_ref(&sym(IO), &sym(OUT_STRING));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let arg = fn_val.get_last_param().unwrap().into_pointer_value();
        let pointee_ty = self.context.get_struct_type("String").unwrap();
        // Get String array from second field of *String
        let to_print_pointer = self
            .builder
            .build_struct_gep(pointee_ty, arg, STRING_CONTENT_IND, "gep")
            .unwrap();

        let to_print_0 = BasicMetadataValueEnum::PointerValue(to_print_pointer);
        let puts_fn = self.module.get_function("puts").unwrap();
        self.builder
            .build_call(puts_fn, &[to_print_0], "call_puts")
            .unwrap();

        let body_val = self.codegen(&Expr::new(&sym("Object")));
        self.builder.build_return(Some(&body_val)).unwrap();
        fn_val.verify(false);
    }

    fn code_io_in_string(&self) {
        let fn_name = method_ref(&sym(IO), &sym(IN_STRING));
        let (_fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let buff_size = self.context.i32_type().const_int(MAX_IN_STRING_LEN, false);
        let array_type = self
            .context
            .i8_type()
            .array_type(MAX_IN_STRING_LEN.try_into().unwrap());
        let dest_ptr = self
            .builder
            .build_array_malloc(self.context.i8_type(), buff_size, "buffer_ptr")
            .unwrap();
        // check this?
        let _ = self
            .builder
            .build_call(
                self.module.get_function("gets").unwrap(),
                &[dest_ptr.into()],
                "_",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap();

        let result = self.code_new_string_from_ptr(dest_ptr, array_type, buff_size);

        // let null = self.context.ptr_type(self.aspace).const_null();
        // self.builder.build_return(Some(&null)).unwrap();
        self.builder.build_return(Some(&result)).unwrap();
    }

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
            let alloca = self
                .builder
                .build_alloca(self.context.ptr_type(self.aspace), &arg_name)
                .unwrap();

            self.builder.build_store(alloca, arg).unwrap();

            self.variables.add_binding(&all_parameters[i].name, &alloca);
        }

        self.current_fn = Some(sym(&fn_name));
        let body_val = self.codegen(body);
        self.builder.build_return(Some(&body_val)).unwrap();
        self.variables.exit_scope();
        fn_val.verify(true);
    }

    fn code_native_method_bodies(&mut self) {
        self.declare_puts();
        self.declare_gets();
        self.declare_strcmp();
        self.code_io_out_string();
        self.code_io_in_string();
        // TEMPORARY!  Just so we can run our program before all natives are really implemented.
        let skip_list = vec![sym("out_string"), sym("in_string")];
        self.code_native_method_stubs(skip_list);
    }

    fn code_method_bodies_for_class(&mut self, cls: &Sym) {
        let methods = self.ct.class_methods.get(cls).unwrap().clone();
        self.current_class = Some(cls.to_owned());
        for (method, ((parameters, return_type), body)) in methods.iter() {
            self.code_method_body(cls, method, parameters, return_type, body);
        }
        self.current_class = None;
    }

    fn code_native_method_stubs(&mut self, skip_list: Vec<Sym>) {
        let native_classes = self.ct.native_classes.clone();
        let stub = &Expr::no_expr();
        for cls in native_classes {
            let methods = self.ct.class_methods.get(&cls).unwrap().clone();
            for (method_name, ((parameters, return_type), _)) in methods.iter() {
                if !skip_list.contains(method_name) {
                    self.code_method_body(&cls, method_name, parameters, return_type, stub);
                }
            }
        }
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

        let return_type = self.context.void_type();
        let fn_type = return_type.fn_type(&[], false);
        let fn_name = "main";
        let fn_val = self.module.add_function(fn_name, fn_type, None);
        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

        // So far the vtables are full of null-pointers.  Install proper function pointers.
        // self.initialize_vtables();

        let main_instance =
            BasicMetadataValueEnum::PointerValue(self.code_new_and_init(&sym("Main")));

        let main_dot_main = self.module.get_function("Main.main").unwrap();

        self.builder
            .build_call(main_dot_main, &[main_instance], "call_Main.main")
            .unwrap();

        self.builder.build_return(None).unwrap();
        fn_val.verify(false);
    }
}
