use crate::codegen::CodeGenManager;
use crate::symbol::Sym;

use inkwell::types::BasicTypeEnum;

// Declare and Code Program Class Structs
impl CodeGenManager<'_> {
    pub fn code_native_class_structs(&self) {
        // class_id
        let object_attrs = &[self.i32_ty.into()];
        self.cl_object_ty.set_body(object_attrs, false);

        // class_id
        let io_attrs = &[self.i32_ty.into()];
        self.cl_io_ty.set_body(io_attrs, false);

        // class_id, value
        let int_attrs = &[self.i32_ty.into(), self.i32_ty.into()];
        self.cl_int_ty.set_body(int_attrs, false);

        // class_id, value
        let bool_attrs = &[self.i32_ty.into(), self.bool_ty.into()];
        self.cl_bool_ty.set_body(bool_attrs, false);

        // class_id, ptr to int for length, str array ptr
        let string_attrs = &[self.i32_ty.into(), self.ptr_ty.into(), self.ptr_ty.into()];
        self.cl_string_ty.set_body(string_attrs, false);
    }

    fn code_program_class_struct(&self, name: &Sym) {
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
            self.code_program_class_struct(class)
        }
    }
}
