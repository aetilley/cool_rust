use crate::ast::{Expr, ExprData, Formal, Program};
use crate::class_table::ClassTable;
use crate::env::Env;
use crate::symbol::{dump_ints, dump_strings, sym, Sym};
use either::Either::Left;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, FunctionType};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::AddressSpace;

use crate::codegen_constants::*;

pub struct CodeGenManager<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub variables: Env<PointerValue<'ctx>>,
    pub aspace: AddressSpace,
    pub ct: ClassTable,
    pub current_class: Option<Sym>,
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

        let mut man = CodeGenManager {
            context,
            builder,
            module,
            variables,
            aspace,
            ct,
            current_class,
        };

        //
        man.code_all_class_structs();
        //
        man.code_all_method_declarations();
        //
        man.code_vtables();
        //
        man.register_globals();
        //
        man.code_all_inits();
        //
        man.code_all_method_bodies();

        man.code_main();

        man
    }

    // Declare and Code Class Structs

    fn declare_native_class_struct_types(&self) {
        self.context.opaque_struct_type("Object");
        self.context.opaque_struct_type("IO");
        self.context.opaque_struct_type("String");
        self.context.opaque_struct_type("Int");
        self.context.opaque_struct_type("Bool");
    }

    fn code_native_class_structs(&self) {
        // vtable
        let object_attrs = &[BasicTypeEnum::PointerType(
            self.context.ptr_type(self.aspace),
        )];
        let typ = self.context.get_struct_type("Object").unwrap();
        typ.set_body(object_attrs, false);

        // vtable, value
        let io_attrs = &[BasicTypeEnum::PointerType(
            self.context.ptr_type(self.aspace),
        )];
        let typ = self.context.get_struct_type("IO").unwrap();
        typ.set_body(io_attrs, false);

        // vtable, value
        let int_attrs = &[
            BasicTypeEnum::PointerType(self.context.ptr_type(self.aspace)),
            BasicTypeEnum::IntType(self.context.i32_type()),
        ];
        let typ = self.context.get_struct_type("Int").unwrap();
        typ.set_body(int_attrs, false);

        // vtable, value
        let bool_attrs = &[
            BasicTypeEnum::PointerType(self.context.ptr_type(self.aspace)),
            BasicTypeEnum::IntType(self.context.bool_type()),
        ];
        let typ = self.context.get_struct_type("Bool").unwrap();
        typ.set_body(bool_attrs, false);

        // vtable, str length, str content
        let string_attrs = &[
            BasicTypeEnum::PointerType(self.context.ptr_type(self.aspace)),
            BasicTypeEnum::StructType(self.context.get_struct_type("Int").unwrap()),
            BasicTypeEnum::ArrayType(self.context.i8_type().array_type(0)),
        ];
        let typ = self.context.get_struct_type("String").unwrap();
        typ.set_body(string_attrs, false);
    }

    fn sym_to_llvm_type(&self, typ: Sym) -> BasicTypeEnum {
        if self.ct.native_classes.contains(&typ) || self.ct.program_classes.contains(&typ) {
            return BasicTypeEnum::StructType(
                self.context
                    .get_struct_type(&typ)
                    .unwrap_or_else(|| panic!("No struct type for {}", typ)),
            );
        }
        panic!("Unknown type {}.", typ);
    }

    fn code_class_struct(&self, name: &Sym) {
        let mut all_attr_types = vec![BasicTypeEnum::PointerType(
            self.context.ptr_type(self.aspace),
        )];
        let other_attr_types_vec: Vec<BasicTypeEnum> = self
            .ct
            .get_all_attrs(name)
            .iter()
            .map(|(_name, typ, _)| self.sym_to_llvm_type(typ.clone()))
            .collect();
        // vtable
        all_attr_types.extend(other_attr_types_vec);
        let typ = self.context.get_struct_type(name).unwrap();
        typ.set_body(&all_attr_types, false);
    }

    pub fn code_all_class_structs(&self) {
        // Must do all declarations before doing any coding.

        // Declarations of native class structs...
        self.declare_native_class_struct_types();
        // ...and non-native class structs types.
        for class in self.ct.program_classes.iter() {
            self.context.opaque_struct_type(class);
        }

        // Coding of native class structs...
        self.code_native_class_structs();
        // ...and non-native class structs types.
        for class in self.ct.program_classes.iter() {
            self.code_class_struct(class)
        }
    }

    // Code function stubs.

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

    fn get_function_type_from_signature(
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

    //
    //
    // VTables

    fn code_vtable_array_for_class(&self, cls: &Sym) -> ArrayValue<'ctx> {
        let table_size: usize = self.ct.get_max_vtable_size();

        let mut initializer_fields = vec![];
        let method_order = self.ct.class_method_order.get(cls).unwrap();
        let vtable_map = self.ct.class_vtable.get(cls).unwrap();
        for method_name in method_order.iter() {
            let resolution_class = vtable_map.get(method_name).unwrap();
            let fn_name = method_ref(resolution_class, method_name);
            let fn_val = self
                .module
                .get_function(&fn_name)
                .unwrap_or_else(|| panic!("No function {}", fn_name));
            let ptr_val = fn_val.as_global_value().as_pointer_value();
            initializer_fields.push(ptr_val);
        }
        // Pad out the fields.
        while initializer_fields.len() < table_size {
            initializer_fields.push(self.context.ptr_type(self.aspace).const_null())
        }

        self.context
            .ptr_type(self.aspace)
            .const_array(&initializer_fields[..])
    }

    fn code_vtable_global_for_class(&mut self, cls: &Sym) {
        let initializer = self.code_vtable_array_for_class(cls);

        let vtable_name = &format!("{}_vtable", cls);

        let vtable_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), vtable_name);

        vtable_global.set_initializer(&initializer);
    }

    pub fn code_vtables(&mut self) {
        let natives_classes = self.ct.native_classes.clone();
        for cls in natives_classes.iter() {
            self.code_vtable_global_for_class(cls);
        }
        let program_classes = self.ct.program_classes.clone();
        for cls in program_classes.iter() {
            self.code_vtable_global_for_class(cls);
        }
    }

    pub fn register_global_for_int(&mut self, i: &Sym) {
        let i_str: &str = &i[..];
        let i_int: u64 = i_str.parse().unwrap();
        let i_val = self.context.i32_type().const_int(i_int, false);
        let int_vtable_ptr = self
            .module
            .get_global(&vtable_ref(&sym("Int")))
            .unwrap()
            .as_pointer_value();

        let initializer = self
            .context
            .const_struct(&[int_vtable_ptr.into(), i_val.into()], false);

        let global_name = &global_int_ref(i);

        let int_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), global_name);

        int_global.set_initializer(&initializer);
    }

    pub fn register_global_for_string(&mut self, s: &Sym) {
        // Content
        let str_content = self.code_array_value_from_sym(s);
        // Vtable
        let str_vtable_ptr = self
            .module
            .get_global(&vtable_ref(&sym("String")))
            .unwrap()
            .as_pointer_value();

        // Len as struct Int
        let int_vtable_ptr = self
            .module
            .get_global(&vtable_ref(&sym("Int")))
            .unwrap()
            .as_pointer_value();
        let str_len = str_content.get_type().len().into();
        let str_len_val = self.context.i32_type().const_int(str_len, false);
        let str_len_struct = self
            .context
            .const_struct(&[int_vtable_ptr.into(), str_len_val.into()], false);

        let initializer = self.context.const_struct(
            &[
                str_vtable_ptr.into(),
                str_len_struct.into(),
                str_content.into(),
            ],
            false,
        );

        let global_name = &global_string_ref(s);

        let int_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), global_name);

        int_global.set_initializer(&initializer);
    }

    // Global data
    pub fn register_globals(&mut self) {
        for i in dump_ints().iter() {
            self.register_global_for_int(i);
        }

        for s in dump_strings().iter() {
            self.register_global_for_string(s);
        }
    }

    // Class Initialization function bodies

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str, fn_value: FunctionValue) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = fn_value.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder
            .build_alloca(self.context.ptr_type(self.aspace), name)
            .unwrap()
    }

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
        // let arg_name = "self";
        // let self_alloca = self.create_entry_block_alloca(arg_name, fn_val);
        // self.builder.build_store(self_alloca, first).unwrap();
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

    // Methods

    fn code_io_out_string(&self) {
        // IO.out_string
        let fn_name = "IO.out_string";
        let fn_val = self.module.get_function(fn_name).unwrap();
        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

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

    fn declare_puts(&self) {
        let input_type = BasicMetadataTypeEnum::PointerType(self.context.ptr_type(self.aspace));
        let puts_type = self.context.i32_type().fn_type(&[input_type], false);
        self.module
            .add_function("puts", puts_type, Some(Linkage::External));
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
            let alloca = self.create_entry_block_alloca(&arg_name, fn_val);

            self.builder.build_store(alloca, arg).unwrap();

            self.variables.add_binding(&all_parameters[i].name, &alloca);
        }

        let body_val = self.codegen(body);
        self.builder.build_return(Some(&body_val)).unwrap();
        self.variables.exit_scope();
        fn_val.verify(true);
    }

    fn code_native_method_bodies(&mut self) {
        self.declare_puts();
        self.code_io_out_string();
        // TEMPORARY!  Just so we can run our program.
        let skip_list = vec![sym("out_string")];
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

    //
    //
    //
    // Expr Codegen

    fn code_new_string(&self, str_array: ArrayValue) -> PointerValue<'ctx> {
        let new_ptr = self.code_new_and_init(&sym("String"));

        let pointee_ty = self.context.get_struct_type("String").unwrap();
        // set Length
        let value = self
            .context
            .i32_type()
            .const_int(str_array.get_type().len() as u64, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, new_ptr, STRING_LEN_IND, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();

        // set array
        let field = self
            .builder
            .build_struct_gep(pointee_ty, new_ptr, STRING_CONTENT_IND, "gep")
            .unwrap();
        self.builder.build_store(field, str_array).unwrap();

        new_ptr
    }

    // fn code_new_int(&self, int_val: IntValue) -> PointerValue<'ctx> {
    //     let new_ptr = self.code_new_and_init(&sym("Int"));

    //     let field = self
    //         .builder
    //         .build_struct_gep(
    //             self.context.get_struct_type("Int").unwrap(),
    //             new_ptr,
    //             INT_VAL_IND,
    //             "gep",
    //         )
    //         .unwrap();
    //     self.builder.build_store(field, int_val).unwrap();

    //     new_ptr
    // }

    fn code_new_and_init(&self, typ: &Sym) -> PointerValue<'ctx> {
        let struct_type = self
            .context
            .get_struct_type(typ)
            .unwrap_or_else(|| panic!("No type {} declared.", typ));
        let malloc_name = &format!("{}_malloc", typ);
        let new_ptr = self.builder.build_malloc(struct_type, malloc_name).unwrap();
        let init_name = &format!("{}_init", typ);
        let initializer = self
            .module
            .get_function(init_name)
            .unwrap_or_else(|| panic!("No initializer found for type {}.", typ));
        let self_arg = BasicMetadataValueEnum::PointerValue(new_ptr);
        let call_name = &format!("{}_init_call", typ);
        self.builder
            .build_call(initializer, &[self_arg], call_name)
            .unwrap();
        new_ptr
    }

    fn code_array_value_from_sym(&self, s: &Sym) -> ArrayValue<'ctx> {
        let array_values: Vec<IntValue> = s
            .as_bytes()
            .iter()
            .map(|byt| self.context.i8_type().const_int(*byt as u64, false))
            .collect();
        self.context.i8_type().const_array(&array_values[..])
    }

    pub fn codegen(&self, expr: &Expr) -> PointerValue {
        let data = &*expr.data;
        match data {
            ExprData::NoExpr {} => self.context.ptr_type(self.aspace).const_null(),
            ExprData::Object { id } => {
                // First check stack then in any class attributes
                if let Some(alloc_ptr) = self.variables.lookup(id) {
                    let ptr = self
                        .builder
                        .build_load(self.context.ptr_type(self.aspace), alloc_ptr, "arg_ptr")
                        .unwrap()
                        .into_pointer_value();
                    return ptr;
                };

                let cls = self.current_class.clone().unwrap();
                let attrs: Vec<(Sym, Sym, Expr)> = self.ct.get_all_attrs(&cls);
                let pointee_ty = self.context.get_struct_type(&cls).unwrap();
                if let Some(offset) = attrs.iter().position(|(name, _, _)| name == id) {
                    let field_offset: u32 =
                        <usize as TryInto<u32>>::try_into(offset).unwrap() + OBJECT_PREFIX_SIZE;
                    let self_ptr_alloc = self.variables.lookup(&sym(SELF)).unwrap();
                    let self_ptr = self
                        .builder
                        .build_load(
                            self.context.ptr_type(self.aspace),
                            self_ptr_alloc,
                            "self_ptr",
                        )
                        .unwrap()
                        .into_pointer_value();
                    let attr_field = self
                        .builder
                        .build_struct_gep(
                            pointee_ty,
                            self_ptr,
                            field_offset,
                            "get attr field from objectname",
                        )
                        .unwrap();
                    return self
                        .builder
                        .build_load(
                            self.context.ptr_type(self.aspace),
                            attr_field,
                            "load attr pointer",
                        )
                        .unwrap()
                        .into_pointer_value();
                }
                panic!("No identifier {} in scope.", id);
            }
            ExprData::StrConst { val } => {
                let global_name = global_string_ref(val);
                // We may want to catch cases where the global is not found and do a malloc.  For now we panic.
                self.module
                    .get_global(&global_name)
                    .unwrap_or_else(|| panic!("No static string found for '{}'", val))
                    .as_pointer_value()
            }
            ExprData::Block { exprs } => {
                let mut result: PointerValue = self.context.ptr_type(self.aspace).const_null();
                for expr in exprs.iter() {
                    result = self.codegen(expr);
                }
                result
            }

            ExprData::IntConst { val } => {
                let global_name = global_int_ref(val);
                // We may want to catch cases where the global is not found and do a malloc.  For now we panic.
                self.module
                    .get_global(&global_name)
                    .unwrap_or_else(|| panic!("No static string found for '{}'", val))
                    .as_pointer_value()
            }
            ExprData::New { typ } => self.code_new_and_init(typ),

            ExprData::Dispatch {
                slf,
                method_name,
                args,
            } => {
                let slf_arg = self.codegen(slf);

                // The following uses the static type not the dynamic type,
                // but the desired function will have the same tag in the vtable
                // for all subtypes, and the same function signature.
                let static_type = slf.stype.clone();

                let vtable_offset: u32 = self
                    .ct
                    .class_method_order
                    .get(&static_type)
                    .unwrap()
                    .iter()
                    .position(|r| r == method_name)
                    .unwrap()
                    .try_into()
                    .unwrap();

                // Note we are guaranteed that the initial segment
                // of the struct will be the layout of static_type.
                // In fact we only need the first field which is the same in all
                // classes.
                let pointee_ty = self.context.get_struct_type(&static_type).unwrap();
                let vtable_pointer_field = self
                    .builder
                    .build_struct_gep(pointee_ty, slf_arg, VTABLE_IND, "get_vtable_pointer_field")
                    .unwrap();

                let vtable_pointer = self
                    .builder
                    .build_load(
                        self.context.ptr_type(self.aspace),
                        vtable_pointer_field,
                        "get vtable pointer",
                    )
                    .unwrap()
                    .into_pointer_value();

                let vtable_type = self
                    .context
                    .ptr_type(self.aspace)
                    .array_type(self.ct.get_max_vtable_size().try_into().unwrap());

                let vtable_array = self
                    .builder
                    .build_load(vtable_type, vtable_pointer, "load vtable")
                    .unwrap()
                    .into_array_value();

                let method_pointer = self
                    .builder
                    .build_extract_value(vtable_array, vtable_offset, "extract method from vtable")
                    .unwrap()
                    .into_pointer_value();

                // Compile the arguments.
                let ((parameters, return_type), _) =
                    self.ct.get_method(&static_type, method_name).unwrap();

                let mut all_parameters = vec![Formal::formal("self", "SELF_TYPE")];
                all_parameters.extend(parameters.to_owned());
                let fn_type =
                    self.get_function_type_from_signature(&all_parameters[..], &return_type);

                let fn_name = &method_ref(&sym("<Dynamic>"), method_name);

                let mut compiled_args: Vec<PointerValue> = vec![slf_arg];
                for arg in args.iter() {
                    let compiled_arg = self.codegen(arg);
                    compiled_args.push(compiled_arg);
                }
                let md_args: Vec<BasicMetadataValueEnum> = compiled_args
                    .into_iter()
                    .map(BasicMetadataValueEnum::PointerValue)
                    .collect();
                let call_name = &format!("{}_call", fn_name);

                // Call the function.
                let result = self
                    .builder
                    .build_indirect_call(fn_type, method_pointer, &md_args, call_name)
                    .unwrap()
                    .try_as_basic_value();

                match result {
                    Left(BasicValueEnum::PointerValue(ptr)) => ptr,
                    _ => self.context.ptr_type(self.aspace).const_null(),
                }
            }

            ExprData::StaticDispatch {
                typ,
                method_name,
                args,
                slf,
            } => {
                let slf_arg = self.codegen(slf);
                let mut compiled_args: Vec<PointerValue> = vec![slf_arg];
                for arg in args.iter() {
                    let compiled_arg = self.codegen(arg);
                    compiled_args.push(compiled_arg);
                }
                let fn_name = &method_ref(typ, method_name);
                let fn_val = self.module.get_function(fn_name).unwrap();
                let md_args: Vec<BasicMetadataValueEnum> = compiled_args
                    .into_iter()
                    .map(BasicMetadataValueEnum::PointerValue)
                    .collect();
                let call_name = &format!("{}_call", fn_name);

                let result = self
                    .builder
                    .build_call(fn_val, &md_args[..], call_name)
                    .unwrap()
                    .try_as_basic_value();
                match result {
                    Left(BasicValueEnum::PointerValue(ptr)) => ptr,
                    _ => self.context.ptr_type(self.aspace).const_null(),
                }
            }

            _ => panic!("codegen not yet supported for ExprData variant {:?}", data),
        }
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
    fn test_codegen_string() {
        let context = Context::create();
        let program = Program::parse("class Main{main():Object{0};};").unwrap();
        let expr = Expr::int_const("42");
        let man = CodeGenManager::from(&context, &program);
        let result = man.codegen(&expr);
        assert_eq!(result.get_type(), man.context.ptr_type(man.aspace));
    }
}
