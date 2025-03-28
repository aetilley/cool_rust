use crate::ast::ExprData;
use crate::symbol::{sym, Sym};
use inkwell::values::{IntValue, PointerValue};

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

// Class Initialization function bodies

impl<'ctx> CodeGenManager<'ctx> {
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

        // set ptr

        let ptr = self.code_array_ptr_from_sym(&sym(""));
        let field = self
            .builder
            .build_struct_gep(self.cl_string_ty, self_alloca, STRING_CONTENT_IND, "gep")
            .unwrap();
        self.builder.build_store(field, ptr).unwrap();
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
