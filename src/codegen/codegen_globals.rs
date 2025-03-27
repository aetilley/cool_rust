use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;
use crate::symbol::{dump_ints, dump_strings, sym, Sym};

use inkwell::types::{BasicTypeEnum, VectorType};
use inkwell::values::PointerValue;
use inkwell::values::{ArrayValue, IntValue};

// Declare and Code Program Class Structs
impl CodeGenManager<'_> {
    fn sym_to_llvm_type(&self, typ: &Sym) -> BasicTypeEnum {
        if self.ct.native_classes.contains(typ) || self.ct.program_classes.contains(typ) {
            return BasicTypeEnum::StructType(
                self.context
                    .get_struct_type(typ)
                    .unwrap_or_else(|| panic!("No struct type for {}", typ)),
            );
        }
        panic!("Cannot convert Sym {} to type", typ);
    }

    fn code_class_struct(&self, name: &Sym) {
        // Start with class_id
        let mut all_attr_types: Vec<BasicTypeEnum> = vec![self.i32_ty.into()];

        let class_attrs = self.ct.get_all_attrs(name);
        let field_types: Vec<BasicTypeEnum> = vec![self.ptr_ty.into(); class_attrs.len()];

        all_attr_types.extend(field_types);
        let typ = self.context.get_struct_type(name).unwrap();
        typ.set_body(&all_attr_types, false);
    }

    pub fn code_program_class_structs(&self) {
        // Must do all declarations before doing any coding.
        for class in self.ct.program_classes.iter() {
            self.context.opaque_struct_type(class);
        }

        for class in self.ct.program_classes.iter() {
            self.code_class_struct(class)
        }
    }
}

// VTables
impl<'ctx> CodeGenManager<'ctx> {
    fn get_vtable_array_for_class(&self, cls: &Sym) -> ArrayValue<'ctx> {
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
            initializer_fields.push(self.ptr_ty.const_null())
        }

        self.ptr_ty.const_array(&initializer_fields[..])
    }

    fn code_vtable_global_for_class(&mut self, cls: &Sym) -> PointerValue<'ctx> {
        let initializer = self.get_vtable_array_for_class(cls);

        let vtable_name = &format!("{}_vtable", cls);

        let vtable_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), vtable_name);

        vtable_global.set_initializer(&initializer);

        vtable_global.as_pointer_value()
    }

    pub fn code_vtable_master_vector(&mut self) {
        // We make a global array whose ith element is a pointer to the vtable
        // for the class with class_id i.
        let mut initializer_fields = vec![];

        let class_id_order: Vec<Sym> = self.ct.class_id_order.clone();

        for cls in class_id_order.iter() {
            let vtable_ptr = self.code_vtable_global_for_class(cls);
            initializer_fields.push(vtable_ptr);
        }

        let initializer = VectorType::const_vector(&initializer_fields[..]);

        let master_vector_global = self.module.add_global(
            initializer.get_type(),
            Some(self.aspace),
            VTABLE_MASTER_VECTOR,
        );

        master_vector_global.set_initializer(&initializer);
    }
}

// Parent table
impl CodeGenManager<'_> {
    pub fn code_parent_vector(&mut self) {
        // We make a global array whose ith element is a pointer to the vtable
        // for the class with class_id i.
        let mut initializer_fields: Vec<IntValue> = vec![];

        let class_id_order: Vec<Sym> = self.ct.class_id_order.clone();
        for cls in class_id_order.iter() {
            let parent_id = if cls == &sym("Object") {
                self.i32_ty
                    .const_int(class_id_order.len().try_into().unwrap(), false)
            } else {
                self.sym_to_class_id_int_val(&self.ct.class_parent[cls])
            };
            initializer_fields.push(parent_id);
        }

        let initializer = VectorType::const_vector(&initializer_fields[..]);

        let parent_vector_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), PARENT_VECTOR);

        parent_vector_global.set_initializer(&initializer);
    }
}

// Static constants
impl CodeGenManager<'_> {
    pub fn register_globals_for_boolean(&mut self, b: bool) {
        let b_int: u64 = b.into();
        let b_val = self.bool_ty.const_int(b_int, false);
        let initializer = self.context.const_struct(
            &[
                self.sym_to_class_id_int_val(&sym(BOOL)).into(),
                b_val.into(),
            ],
            false,
        );

        let global_name = &global_bool_ref(b);

        let bool_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), global_name);

        bool_global.set_initializer(&initializer);
    }

    pub fn register_global_for_int(&mut self, i: &Sym) {
        let i_str: &str = &i[..];
        let i_int: u64 = i_str.parse().unwrap();
        let i_val = self.i32_ty.const_int(i_int, false);

        let initializer = self.context.const_struct(
            &[self.sym_to_class_id_int_val(&sym(INT)).into(), i_val.into()],
            false,
        );

        let global_name = &global_int_ref(i);

        let int_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), global_name);

        int_global.set_initializer(&initializer);
    }

    // Must do this after globals for ints.
    pub fn register_global_for_string(&mut self, s: &Sym) {
        // Content
        let str_content = self.code_array_value_from_sym(s);
        // Len as struct Int

        let len_str = &format!("{}", s.len());
        let str_len_struct_ptr = self
            .module
            .get_global(&global_int_ref(&sym(len_str)))
            .unwrap()
            .as_pointer_value();

        let initializer = self.context.const_struct(
            &[
                self.sym_to_class_id_int_val(&sym(STRING)).into(),
                str_len_struct_ptr.into(),
                str_content.into(),
            ],
            false,
        );

        let global_name = &global_string_ref(s);

        let string_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), global_name);

        string_global.set_initializer(&initializer);
    }

    pub fn register_globals(&mut self) {
        for i in dump_ints().iter() {
            self.register_global_for_int(i);
        }

        for s in dump_strings().iter() {
            let len_str = &format!("{}", s.len());
            self.register_global_for_int(&sym(len_str));
            self.register_global_for_string(s);
        }

        self.register_globals_for_boolean(true);
        self.register_globals_for_boolean(false);
    }
}
