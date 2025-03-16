use std::collections::hash_map::HashMap;

use crate::ast::{Expr, ExprData, Formal};
use crate::class_table::ClassTable;
use crate::symbol::{sym, Sym};
use either::Either::Left;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{
    ArrayValue, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::AddressSpace;

pub struct CodeGenManager<'ctx> {
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub context: &'ctx Context,
    pub ct: ClassTable,
    pub variables: HashMap<Sym, PointerValue<'ctx>>,
}

impl<'ctx> CodeGenManager<'ctx> {
    pub fn init(context: &'ctx Context, ct: ClassTable) -> Self {
        let module = context.create_module("cool_module");
        let builder = context.create_builder();
        let variables = HashMap::<Sym, PointerValue>::new();

        CodeGenManager {
            builder,
            module,
            context,
            ct,
            variables,
        }
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
        let typ = self.context.get_struct_type("Object").unwrap();
        typ.set_body(&[], false);

        let typ = self.context.get_struct_type("IO").unwrap();
        typ.set_body(&[], false);

        let int_attrs = &[BasicTypeEnum::IntType(self.context.i32_type())];
        let typ = self.context.get_struct_type("Int").unwrap();
        typ.set_body(int_attrs, false);

        let bool_attrs = &[BasicTypeEnum::IntType(self.context.bool_type())];
        let typ = self.context.get_struct_type("Bool").unwrap();
        typ.set_body(bool_attrs, false);

        let string_attrs = &[
            BasicTypeEnum::StructType(self.context.get_struct_type("Int").unwrap()),
            BasicTypeEnum::ArrayType(self.context.i8_type().array_type(0)),
        ];
        let typ = self.context.get_struct_type("String").unwrap();
        typ.set_body(string_attrs, false);
    }

    fn sym_to_llvm_type(&self, typ: Sym) -> BasicTypeEnum {
        if self.ct.native_classes.contains(&typ) || self.ct.classes.contains(&typ) {
            return BasicTypeEnum::StructType(
                self.context
                    .get_struct_type(typ.as_str())
                    .unwrap_or_else(|| panic!("No struct type for {}", typ)),
            );
        }
        panic!("Unknown type {}.", typ);
    }

    fn code_class_struct(&self, name: Sym) {
        let attr_types_vec: Vec<BasicTypeEnum> = self
            .ct
            .get_all_attrs(name)
            .iter()
            .map(|(_name, typ, _)| self.sym_to_llvm_type(*typ))
            .collect();
        let attr_types = &attr_types_vec[..];
        let typ = self.context.get_struct_type(name.as_str()).unwrap();
        typ.set_body(attr_types, false);
    }

    pub fn code_all_class_structs(&self) {
        // Must do all declarations before doing any coding.

        // Declarations of native class structs...
        self.declare_native_class_struct_types();
        // ...and non-native class structs types.
        for class in self.ct.classes.iter() {
            self.context.opaque_struct_type(class.as_str());
        }

        // Coding of native class structs...
        self.code_native_class_structs();
        // ...and non-native class structs types.
        for class in self.ct.classes.iter() {
            self.code_class_struct(*class)
        }
    }

    // Global data
    pub fn register_globals(&mut self) {
        // Default Int
        let zero = BasicValueEnum::IntValue(self.context.i32_type().const_int(0, false));
        let struct_type = self.context.get_struct_type("Int").unwrap();
        let ptr =
            self.module
                .add_global(struct_type, Some(AddressSpace::default()), "_DEFAULT_Int");
        let struct_value = struct_type.const_named_struct(&[zero]);
        ptr.set_initializer(&struct_value);

        // Default Bool
        let fls = BasicValueEnum::IntValue(self.context.bool_type().const_int(0, false));
        let struct_type = self.context.get_struct_type("Bool").unwrap();
        let ptr =
            self.module
                .add_global(struct_type, Some(AddressSpace::default()), "_DEFAULT_Bool");
        let struct_value = struct_type.const_named_struct(&[fls]);
        ptr.set_initializer(&struct_value);

        // Default String
        let zero_len = BasicValueEnum::IntValue(self.context.i32_type().const_int(0, false));
        let empty_string = BasicValueEnum::ArrayValue(self.context.i8_type().const_array(&[]));
        let struct_type = self.context.get_struct_type("String").unwrap();
        let ptr = self.module.add_global(
            struct_type,
            Some(AddressSpace::default()),
            "_DEFAULT_String",
        );
        let struct_value = struct_type.const_named_struct(&[zero_len, empty_string]);
        ptr.set_initializer(&struct_value);
    }

    // Class Initialization functions

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str, fn_value: FunctionValue) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = fn_value.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder
            .build_alloca(self.context.ptr_type(AddressSpace::default()), name)
            .unwrap()
    }

    fn make_code_init(&self, name: Sym, code_body: fn(&CodeGenManager<'ctx>, &str, PointerValue)) {
        // Takes care of boilerplate function setup and calls `code_body`.
        let self_type_inner = self.context.ptr_type(AddressSpace::default());
        let self_type = BasicMetadataTypeEnum::PointerType(self_type_inner);
        let return_type = self.context.void_type();
        let fn_type = return_type.fn_type(&[self_type], false);
        let fn_name = &format!("{}_init", name);
        let fn_val = self.module.add_function(fn_name, fn_type, None);
        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

        let first = fn_val.get_first_param().unwrap();
        let arg_name = "self";
        let self_alloca = self.create_entry_block_alloca(arg_name, fn_val);
        self.builder.build_store(self_alloca, first).unwrap();
        code_body(self, name.as_str(), self_alloca);
        self.builder.build_return(None).unwrap();
        fn_val.verify(false);
    }

    fn code_empty_init_body(&self, _name: &str, _self_alloca: PointerValue) {}

    fn code_int_init_body(&self, _name: &str, self_alloca: PointerValue) {
        let pointee_ty = self.context.get_struct_type("Int").unwrap();
        // set val = 0
        let value = self.context.i32_type().const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, 0, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_bool_init_body(&self, _name: &str, self_alloca: PointerValue) {
        let pointee_ty = self.context.get_struct_type("Bool").unwrap();
        // set val = false
        let value = self.context.bool_type().const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, 0, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_string_init_body(&self, _name: &str, self_alloca: PointerValue) {
        let pointee_ty = self.context.get_struct_type("String").unwrap();
        // set Length = 0
        let value = self.context.i32_type().const_int(0, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, 0, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();

        // set ptr -> ""

        let value = self.context.i8_type().const_array(&[]);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, self_alloca, 1, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();
    }

    fn code_native_inits(&self) {
        self.make_code_init(sym("Object"), CodeGenManager::code_empty_init_body);
        self.make_code_init(sym("IO"), CodeGenManager::code_empty_init_body);
        self.make_code_init(sym("Int"), CodeGenManager::code_int_init_body);
        self.make_code_init(sym("Bool"), CodeGenManager::code_bool_init_body);
        self.make_code_init(sym("String"), CodeGenManager::code_string_init_body);
    }

    fn default_value_for_type(&self, typ: Sym) -> PointerValue {
        match typ.as_str() {
            "Int" | "Bool" | "String" => self
                .module
                .get_global(&format!("_DEFAULT_{}", typ.as_str()))
                .unwrap()
                .as_pointer_value(),
            _ => self.context.ptr_type(AddressSpace::default()).const_null(),
        }
    }

    fn code_init_body(&self, class_name: &str, self_alloca: PointerValue) {
        let attrs = self
            .ct
            .class_attrs
            .get(&sym(class_name))
            .unwrap()
            .iter()
            .enumerate();
        for (ind, (_, typ, init)) in attrs {
            let value = if (*init.data == ExprData::NoExpr {}) {
                let inner = self.default_value_for_type(*typ);
                inner
            } else {
                self.codegen(init)
            };
            let pointee_ty = self.context.get_struct_type(class_name).unwrap();
            let field = self
                .builder
                .build_struct_gep(pointee_ty, self_alloca, ind.try_into().unwrap(), "gep")
                .unwrap();
            self.builder.build_store(field, value).unwrap();
        }
    }

    fn code_init_for_class(&self, name: Sym) {
        self.make_code_init(name, CodeGenManager::code_init_body);
    }

    pub fn code_all_inits(&self) {
        self.code_native_inits();

        for class in self.ct.classes.iter() {
            self.code_init_for_class(*class);
        }
    }

    // Methods

    fn code_main(&self) {
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

        let main_instance = BasicMetadataValueEnum::PointerValue(self.code_new(sym("Main")));

        let main_dot_main = self.module.get_function("Main.main").unwrap();

        self.builder
            .build_call(main_dot_main, &[main_instance], "call_Main.main")
            .unwrap();

        self.builder.build_return(None).unwrap();
        fn_val.verify(false);
    }

    fn code_io_out_string(&self) {
        // IO.out_string
        let return_type = self.context.void_type();
        // *String

        let self_arg_type =
            BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default()));
        let input_arg_type =
            BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default()));
        let fn_type = return_type.fn_type(&[self_arg_type, input_arg_type], false);
        let fn_name = "IO.out_string";
        let fn_val = self.module.add_function(fn_name, fn_type, None);
        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

        ////
        // This created a bug, why?
        //let arg_alloca_inner = self.create_entry_block_alloca("input", fn_val);
        //let arg = fn_val.get_first_param().unwrap();
        //self.builder.build_store(arg_alloca_inner, arg).unwrap();
        let arg = fn_val.get_last_param().unwrap().into_pointer_value();
        let pointee_ty = self.context.get_struct_type("String").unwrap();
        // Get String array from second field of *String
        let to_print_pointer = self
            .builder
            .build_struct_gep(pointee_ty, arg, 1, "gep")
            .unwrap();

        let to_print_0 = BasicMetadataValueEnum::PointerValue(to_print_pointer);
        let puts_fn = self.module.get_function("puts").unwrap();
        self.builder
            .build_call(puts_fn, &[to_print_0], "call_puts")
            .unwrap();

        self.builder.build_return(None).unwrap();
        fn_val.verify(false);
    }

    fn declare_puts(&self) {
        let input_type =
            BasicMetadataTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default()));
        let puts_type = self.context.i32_type().fn_type(&[input_type], false);
        self.module
            .add_function("puts", puts_type, Some(Linkage::External));
    }

    fn code_native_methods(&self) {
        self.declare_puts();
        self.code_io_out_string();
        // TODO
        // self.code_io_out_int();
    }

    fn code_program_methods(&mut self) {
        // Make sure not to grab methods for native classes.
        let classes = self.ct.classes.clone();
        for cls in classes {
            let methods = self.ct.class_methods.get(&cls).unwrap().clone();
            for (method, ((parameters, return_type), body)) in methods.iter() {
                self.code_method(cls, *method, parameters, *return_type, body);
            }
        }
    }

    fn code_method(
        &mut self,
        cls: Sym,
        method: Sym,
        parameters: &[Formal],
        _return_type: Sym,
        body: &Expr,
    ) {
        // TODO! Currently ignoring the types of args / return and treating them as generic pointers.

        // Must add self parameter.
        let mut all_parameters = vec![Formal::formal("self", "SELF_TYPE")];
        all_parameters.append(&mut parameters.to_owned());

        let ret_type = self.context.ptr_type(AddressSpace::default());
        let args_types = std::iter::repeat(ret_type)
            .take(all_parameters.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = ret_type.fn_type(&args_types[..], false);
        let fn_name = format!("{}.{}", cls, method);
        let fn_val = self.module.add_function(&fn_name, fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_pointer_value()
                .set_name(all_parameters[i].name.as_str());
        }

        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);

        // Build Env
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let arg_name = all_parameters[i].name.as_str();
            let alloca = self.create_entry_block_alloca(arg_name, fn_val);

            self.builder.build_store(alloca, arg).unwrap();

            self.variables.insert(all_parameters[i].name, alloca);
        }

        let body_val = self.codegen(body);
        self.builder.build_return(Some(&body_val)).unwrap();
        fn_val.verify(true);
    }

    pub fn code_all_methods(&mut self) {
        self.code_native_methods();
        self.code_program_methods();
        self.code_main();
    }

    // Expr Codegen

    fn code_new_string(&self, str_array: ArrayValue) -> PointerValue<'ctx> {
        let main_type = self.context.get_struct_type("String").unwrap();
        let new_ptr = self
            .builder
            .build_malloc(main_type, "String_malloc")
            .unwrap();
        let str_len = str_array.get_type().len();

        let pointee_ty = self.context.get_struct_type("String").unwrap();
        // set Length
        let value = self.context.i32_type().const_int(str_len as u64, false);
        let field = self
            .builder
            .build_struct_gep(pointee_ty, new_ptr, 0, "gep")
            .unwrap();
        self.builder.build_store(field, value).unwrap();

        // set array
        let field = self
            .builder
            .build_struct_gep(pointee_ty, new_ptr, 1, "gep")
            .unwrap();
        self.builder.build_store(field, str_array).unwrap();

        new_ptr
    }

    fn code_new_int(&self, int_val: IntValue) -> PointerValue<'ctx> {
        let int_type = self.context.get_struct_type("Int").unwrap();
        let new_ptr = self.builder.build_malloc(int_type, "int_malloc").unwrap();

        // set Length
        let field = self
            .builder
            .build_struct_gep(int_type, new_ptr, 0, "gep")
            .unwrap();
        self.builder.build_store(field, int_val).unwrap();

        new_ptr
    }

    fn code_new(&self, typ: Sym) -> PointerValue<'ctx> {
        let struct_type = self.context.get_struct_type(typ.as_str()).unwrap();
        let malloc_name = &format!("{}_malloc", typ.as_str());
        let new_ptr = self.builder.build_malloc(struct_type, malloc_name).unwrap();
        let init_name = &format!("{}_init", typ.as_str());
        let initializer = self
            .module
            .get_function(init_name)
            .unwrap_or_else(|| panic!("No initializer found for type {}.", typ.as_str()));
        let self_arg = BasicMetadataValueEnum::PointerValue(new_ptr);
        let call_name = &format!("{}_init_call", typ.as_str());
        self.builder
            .build_call(initializer, &[self_arg], call_name)
            .unwrap();
        new_ptr
    }

    pub fn codegen(&self, expr: &Expr) -> PointerValue {
        let data = &*expr.data;
        match data {
            ExprData::StrConst { val } => {
                let array_values: Vec<IntValue> = val
                    .as_bytes()
                    .iter()
                    .map(|byt| self.context.i8_type().const_int(*byt as u64, false))
                    .collect();
                let inner = self.context.i8_type().const_array(&array_values[..]);
                let new_struct = self.code_new_string(inner);
                new_struct
            }

            ExprData::IntConst { val } => {
                let int_val: u64 = val.as_str().parse().unwrap();

                let inner = self.context.i8_type().const_int(int_val, false);
                let new_struct = self.code_new_int(inner);
                new_struct
            }
            ExprData::New { typ } => self.code_new(*typ),

            // ExprData::Dispatch {
            //     slf,
            //     method_name,
            //     args,
            // } => {
            //     let slf_arg = self.codegen(slf);
            //     // The following uses the static type not the dynamic type,
            //     // but the desired function will have the same tag in the vtable
            //     // for all subtypes.
            //     let vtable_key = class_method_tags(expr.stype, method_name);
            //     let fn_name = slf_arg.vtable.get(vtable_key)

            //     let mut compiled_args: Vec<PointerValue> = vec![slf_arg];
            //     for arg in args.iter() {
            //         let compiled_arg = self.codegen(arg);
            //         compiled_args.push(compiled_arg);
            //     }

            //     let fn_val = self.module.get_function(fn_name).unwrap();
            //     let md_args: Vec<BasicMetadataValueEnum> = compiled_args
            //         .into_iter()
            //         .map(|ptr| BasicMetadataValueEnum::PointerValue(ptr))
            //         .collect();
            //     let call_name = &format!("{}_call", fn_name);
            //     let result = self
            //         .builder
            //         .build_call(fn_val, &md_args[..], call_name)
            //         .unwrap()
            //         .try_as_basic_value();
            //     match result {
            //         Left(BasicValueEnum::PointerValue(ptr)) => ptr,
            //         _ => self.context.ptr_type(AddressSpace::default()).const_null(),
            //     }
            // }
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
                // TODO: should really get these strings programmatically E.g "get_method_ref(class, method_name)"
                let fn_name = &format!("{}.{}", typ, method_name);
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
                    _ => self.context.ptr_type(AddressSpace::default()).const_null(),
                }
            }

            _ => panic!("codegen not yet supported for ExprData variant {:?}", data),
        }
    }
}
