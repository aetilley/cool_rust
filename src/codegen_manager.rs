use crate::class_table::ClassTable;
use crate::symbol::{sym, Sym};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use inkwell::AddressSpace;

pub struct CodeGenManager<'ctx> {
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub context: &'ctx Context,
    pub ct: ClassTable,
}

impl<'ctx> CodeGenManager<'ctx> {
    pub fn init(context: &'ctx Context, ct: ClassTable) -> Self {
        let module = context.create_module("cool_module");
        let builder = context.create_builder();

        CodeGenManager {
            builder,
            module,
            context,
            ct,
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
            // TODO, This is the ascii array.  probably not final. Pointer to Array instead?
            BasicTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
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
            .map(|(_name, typ)| self.sym_to_llvm_type(*typ))
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

    // Class Initialization functions

    fn code_init_from_body<'a>(&'a self, name: Sym, maybe_body: Option<&dyn BasicValue<'a>>) {
        let self_type =
            BasicMetadataTypeEnum::StructType(self.context.get_struct_type(name.as_str()).unwrap());
        let return_type = self.context.void_type();
        let fn_type = return_type.fn_type(&[self_type], false);
        let fn_name = &format!("Init_{}", name);
        let fn_val = self.module.add_function(fn_name, fn_type, None);
        let block_name = &format!("{}_entry", fn_name);
        let entry = self.context.append_basic_block(fn_val, block_name);
        self.builder.position_at_end(entry);
        self.builder.build_return(maybe_body).unwrap();
    }

    fn code_trivial_init(&self, name: Sym) {
        self.code_init_from_body(name, None)
    }

    fn code_native_inits(&self) {
        self.code_trivial_init(sym("Int"));
        self.code_trivial_init(sym("Bool"));
        self.code_trivial_init(sym("String"));
        self.code_trivial_init(sym("IO"));
        self.code_trivial_init(sym("Object"));
    }

    fn code_init_for_class(&self, name: Sym) {
        let body = self.context.i32_type().const_int(42, false);
        self.code_init_from_body(name, Some(&body));
    }

    pub fn code_all_inits(&self) {
        self.code_native_inits();

        for class in self.ct.classes.iter() {
            self.code_init_for_class(*class);
        }
    }

    // Methods (stub)

    pub fn code_all_methods(&self) {
        //    pub fn const_struct(&self, values: &[BasicValueEnum], packed: bool) -> StructValue {

        let arg1 = BasicMetadataTypeEnum::FloatType(self.context.f64_type());
        let io_type = self.context.get_struct_type("String").unwrap();
        let fn_type = io_type.fn_type(&[arg1], false);
        let fn_val = self.module.add_function("EntryFoo", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");

        self.builder.position_at_end(entry);

        //self.fn_value_opt = Some(function);

        // build variables map
        //self.variables.reserve(proto.args.len());

        // for (i, arg) in function.get_param_iter().enumerate() {
        //     let arg_name = proto.args[i].as_str();
        //     let alloca = self.create_entry_block_alloca(arg_name);

        //     self.builder.build_store(alloca, arg).unwrap();

        //     self.variables.insert(proto.args[i].clone(), alloca);
        // }

        // compile body
        let body = self.context.f64_type().const_float(42.0);

        self.builder.build_return(Some(&body)).unwrap();
    }
}
