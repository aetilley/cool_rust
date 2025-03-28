use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;
use crate::symbol::{dump_ints, dump_strings, sym, Sym};

use inkwell::types::VectorType;
use inkwell::values::BasicValueEnum;
use inkwell::values::PointerValue;
use inkwell::values::{ArrayValue, GlobalValue, IntValue};

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

    pub fn code_vtable_master_table(&mut self) {
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
    pub fn code_parent_table(&mut self) {
        // We make a global array whose ith element is the class_id of the parent
        // of the class with class_id i.
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

// Parent table
impl CodeGenManager<'_> {
    pub fn code_struct_size_table(&mut self) {
        // We make a global array whose ith element is the size in bytes of the class with class_id i.
        let mut initializer_fields: Vec<IntValue> = vec![];

        let class_id_order: Vec<Sym> = self.ct.class_id_order.clone();
        for cls in class_id_order.iter() {
            let size = self.module.get_struct_type(cls).unwrap().size_of().unwrap();
            initializer_fields.push(size);
        }

        let initializer = VectorType::const_vector(&initializer_fields[..]);

        let size_vector_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), STRUCT_SIZE_TABLE);

        size_vector_global.set_initializer(&initializer);
    }
}

// Parent table
impl CodeGenManager<'_> {
    pub fn code_type_name_table(&mut self) {
        // We make a global array whose ith element is the name of class with class_id i.
        let mut initializer_fields: Vec<BasicValueEnum> = vec![];

        let class_id_order: Vec<Sym> = self.ct.class_id_order.clone();
        for cls in class_id_order.iter() {
            let ptr = self
                .module
                .get_global(&global_string_ref(cls))
                .unwrap()
                .as_pointer_value();
            initializer_fields.push(ptr.into());
        }

        let type_name_vec = VectorType::const_vector(&initializer_fields[..]);

        let type_name_vec_global = self.module.add_global(
            type_name_vec.get_type(),
            Some(self.aspace),
            TYPE_NAME_VECTOR,
        );

        type_name_vec_global.set_initializer(&type_name_vec);
    }
}

// Static constants
impl CodeGenManager<'_> {
    pub fn register_globals_for_boolean(&self, b: bool) -> GlobalValue {
        // Declare a Cool Bool static constant.
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

        bool_global
    }

    pub fn register_global_for_int(&self, i: &Sym) -> GlobalValue {
        // Declare a Cool Int static constant.
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

        int_global
    }

    pub fn register_global_string_content(&self, s: &Sym) -> GlobalValue {
        // Declare a character array static constant.
        let initializer = self.code_array_value_from_sym(s);
        let glbl = self.module.add_global(
            initializer.get_type(),
            Some(self.aspace),
            &global_str_content_ref(s),
        );
        glbl.set_initializer(&initializer);

        glbl
    }

    // Must do this after globals for ints.

    pub fn register_global_for_string(&self, s: &Sym) -> GlobalValue {
        // Declare a Cool String static constant.
        let class_id = self.sym_to_class_id_int_val(&sym(STRING)).into();

        // Length as a global int.
        let len_str = &format!("{}", s.len());
        //  TODO:  See comment in `register_static_constants`
        let str_len_glbl = match self.module.get_global(&global_int_ref(&sym(len_str))) {
            Some(glbl) => glbl,
            None => self.register_global_for_int(&sym(len_str)),
        }
        .as_pointer_value();

        // Content
        let content_glbl = self.register_global_string_content(s).as_pointer_value();

        let initializer = self
            .context
            .const_struct(&[class_id, str_len_glbl.into(), content_glbl.into()], false);

        let string_global = self.module.add_global(
            initializer.get_type(),
            Some(self.aspace),
            &global_string_ref(s),
        );

        string_global.set_initializer(&initializer);

        string_global
    }

    pub fn register_static_constants(&self) {
        for i in dump_ints().iter() {
            self.register_global_for_int(i);
        }

        for s in dump_strings().iter() {
            self.register_global_for_string(s);
        }

        // TODO:  It would be nice to just top off the string symbol table
        // at the beginning of `register_static_constants` with type names
        // (and the int table with their lengths) instead of including the
        // additional global declaration here and in `register_global_for_string`.
        // Unfortunately it seems that, for whatever reason, our global symbol
        // pool does not allow new entries quite so close to when the
        // pool is dumped (they do not appear in the dump).
        // Until I debug why this is the case (or migrate to a
        // different interning crate) we have to do this separately.
        // and (if we wish to avoid the ugliness of duplicate global int
        // definitions) check for duplicates ints in `register_global_for_string`.
        let native_classes = self.ct.native_classes.clone();
        for s in native_classes.iter() {
            self.register_global_for_string(s);
        }
        let program_classes = self.ct.program_classes.clone();
        for s in program_classes.iter() {
            self.register_global_for_string(s);
        }

        self.register_globals_for_boolean(true);
        self.register_globals_for_boolean(false);
    }
}
