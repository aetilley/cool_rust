use crate::ast::ExprData;
use crate::symbol::{sym, Sym};
use inkwell::values::PointerValue;

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

// Class Initialization function bodies

impl<'ctx> CodeGenManager<'ctx> {
    pub fn code_class_initializer_declaration(&mut self, cls: &Sym) {
        // See codegen_class_inits for definition of class initializers.
        self.code_function_declaration(init_ref(cls), &[], &sym(OBJECT))
    }

    fn make_code_init_body_wrapper(
        &mut self,
        name: &Sym,
        code_body: fn(&mut CodeGenManager<'ctx>, &str, PointerValue<'ctx>),
    ) {
        // Takes care of boilerplate initializer function setup and
        // then calls `code_body`.

        let class_id = self.sym_to_class_id_int_val(name);

        let fn_name = init_ref(name);
        let (fn_val, _) = self.code_function_entry(&fn_name);

        let first = fn_val.get_first_param().unwrap();
        let self_ptr = first.into_pointer_value();

        // Class id
        self.store_int_value_into_pointer_at_struct(self_ptr, name, CLASS_ID_IND, class_id);

        // Code Body
        code_body(self, name, self_ptr);
        let null = self.context.ptr_type(self.aspace).const_null();
        self.builder.build_return(Some(&null)).unwrap();
        fn_val.verify(false);
    }

    fn code_empty_init_body(&mut self, _name: &str, _self_alloca: PointerValue) {}

    fn code_int_init_body(&mut self, name: &str, self_alloca: PointerValue<'ctx>) {
        // set val = 0
        let value = self.i32_ty.const_int(0, false);
        self.store_int_value_into_pointer_at_struct(self_alloca, name, INT_VAL_IND, value);
    }

    fn code_bool_init_body(&mut self, name: &str, self_alloca: PointerValue<'ctx>) {
        // set val = false
        let value = self.bool_ty.const_int(0, false);
        self.store_int_value_into_pointer_at_struct(self_alloca, name, BOOL_VAL_IND, value);
    }

    fn code_string_init_body(&mut self, name: &str, self_alloca: PointerValue<'ctx>) {
        // set Length = 0
        let value = self.i32_ty.const_int(0, false);
        let value_ptr = self.code_new_int(value);
        self.store_pointer_value_into_pointer_at_struct(self_alloca, name, BOOL_VAL_IND, value_ptr);

        // set content to empty string
        let content_ptr = self.code_array_ptr_from_sym(&sym(""));
        self.store_pointer_value_into_pointer_at_struct(
            self_alloca,
            name,
            STRING_CONTENT_IND,
            content_ptr,
        );
    }

    fn code_native_inits(&mut self) {
        self.make_code_init_body_wrapper(&sym("Object"), CodeGenManager::code_empty_init_body);
        self.make_code_init_body_wrapper(&sym("IO"), CodeGenManager::code_empty_init_body);
        self.make_code_init_body_wrapper(&sym("Int"), CodeGenManager::code_int_init_body);
        self.make_code_init_body_wrapper(&sym("Bool"), CodeGenManager::code_bool_init_body);
        self.make_code_init_body_wrapper(&sym("String"), CodeGenManager::code_string_init_body);
    }

    fn code_init_body_for_program_class(&mut self, class_name: &str, self_alloca: PointerValue) {
        // For non-native classes, we have to look up the attributes in the class table.
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

    fn code_init_for_program_class(&mut self, name: &Sym) {
        self.make_code_init_body_wrapper(name, CodeGenManager::code_init_body_for_program_class);
    }

    pub fn code_all_initializer_definitions(&mut self) {
        self.code_native_inits();

        let classes = self.ct.program_classes.clone();
        for cls in classes.iter() {
            self.code_init_for_program_class(cls);
        }
    }
}
