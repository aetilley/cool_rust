use crate::codegen::CodeGenManager;
use crate::codegen_constants::*;
use crate::symbol::{dump_ints, dump_strings, sym, Sym};

use inkwell::values::ArrayValue;
// VTables

impl<'ctx> CodeGenManager<'ctx> {
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
}

// Static constants
impl CodeGenManager<'_> {
    pub fn register_globals_for_boolean(&mut self, b: bool) {
        let b_int: u64 = b.into();
        let b_val = self.context.bool_type().const_int(b_int, false);
        let bool_vtable_ptr = self
            .module
            .get_global(&vtable_ref(&sym("Bool")))
            .unwrap()
            .as_pointer_value();

        let initializer = self
            .context
            .const_struct(&[bool_vtable_ptr.into(), b_val.into()], false);

        let global_name = &global_bool_ref(b);

        let bool_global =
            self.module
                .add_global(initializer.get_type(), Some(self.aspace), global_name);

        bool_global.set_initializer(&initializer);
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

    pub fn register_globals(&mut self) {
        for i in dump_ints().iter() {
            self.register_global_for_int(i);
        }

        for s in dump_strings().iter() {
            self.register_global_for_string(s);
        }

        self.register_globals_for_boolean(true);
        self.register_globals_for_boolean(false);
    }
}
