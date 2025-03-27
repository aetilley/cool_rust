use crate::ast::Cases;
use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;
use crate::symbol::{sym, Sym};
use inkwell::types::{ArrayType, BasicTypeEnum, IntType};
use inkwell::values::{ArrayValue, IntValue};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::IntPredicate;

impl<'ctx> CodeGenManager<'ctx> {
    pub fn code_array_value_from_sym(&self, s: &Sym) -> ArrayValue<'ctx> {
        let array_values: Vec<IntValue> = s
            .as_bytes()
            .iter()
            .map(|byt| self.i8_ty.const_int(*byt as u64, false))
            .collect();
        self.i8_ty.const_array(&array_values[..])
    }

    //pub fn code_new_string(&self, str_array: ArrayValue) -> PointerValue<'ctx> {
    //    let new_ptr = self.code_new_and_init(&sym("String"));

    //    let pointee_ty = self.context.get_struct_type("String").unwrap();
    //    // set Length
    //    let value = self
    //        .context
    //        .i32_type()
    //        .const_int(str_array.get_type().len() as u64, false);
    //    let field = self
    //        .builder
    //        .build_struct_gep(pointee_ty, new_ptr, STRING_LEN_IND, "gep")
    //        .unwrap();
    //    self.builder.build_store(field, value).unwrap();

    //    // set array
    //    let field = self
    //        .builder
    //        .build_struct_gep(pointee_ty, new_ptr, STRING_CONTENT_IND, "gep")
    //        .unwrap();
    //    self.builder.build_store(field, str_array).unwrap();

    //    new_ptr
    //}

    pub fn code_new_string_from_ptr(
        &self,
        str_array_ptr: PointerValue,
        array_type: ArrayType,
    ) -> PointerValue<'ctx> {
        let new_ptr = self.code_new_and_init(&sym("String"));

        // Get length
        let length = self
            .builder
            .build_call(
                self.module.get_function("strlen").unwrap(),
                &[str_array_ptr.into()],
                "_",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();

        let length_struct_ptr = self.code_new_int(length);
        let length_struct = self
            .builder
            .build_load(self.cl_int_ty, length_struct_ptr, "length_struct")
            .unwrap();

        // set Length
        let field = self
            .builder
            .build_struct_gep(self.cl_string_ty, new_ptr, STRING_LEN_IND, "gep")
            .unwrap();
        self.builder.build_store(field, length_struct).unwrap();

        //set array
        let field = self
            .builder
            .build_struct_gep(self.cl_string_ty, new_ptr, STRING_CONTENT_IND, "gep")
            .unwrap();
        let value = self
            .builder
            .build_load(array_type, str_array_ptr, "array")
            .unwrap()
            .into_array_value();
        self.builder.build_store(field, value).unwrap();

        new_ptr
    }

    pub fn code_new_int(&self, int_val: IntValue) -> PointerValue<'ctx> {
        let new_ptr = self.code_new_and_init(&sym("Int"));

        let field = self
            .builder
            .build_struct_gep(self.cl_int_ty, new_ptr, INT_VAL_IND, "gep")
            .unwrap();
        self.builder.build_store(field, int_val).unwrap();

        new_ptr
    }

    pub fn code_new_and_init(&self, typ: &Sym) -> PointerValue<'ctx> {
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
        let call_name = &format!("{}_init_call", typ);
        self.builder
            .build_call(initializer, &[new_ptr.into()], call_name)
            .unwrap();
        new_ptr
    }

    pub fn store_pointer_value_into_pointer_at_struct(
        &self,
        ptr_at_struct: PointerValue<'ctx>,
        struct_type_name: &str,
        field_offset: u32,
        pointer_to_store: PointerValue<'ctx>,
    ) {
        // Basically just a helper function to do a gep then a load.
        let field = self
            .builder
            .build_struct_gep(
                self.context.get_struct_type(struct_type_name).unwrap(),
                ptr_at_struct,
                field_offset,
                "Field",
            )
            .unwrap();
        self.builder.build_store(field, pointer_to_store).unwrap();
    }

    pub fn load_pointer_field_from_pointer_at_struct(
        &self,
        ptr_at_struct: PointerValue<'ctx>,
        struct_type_name: &str,
        field_offset: u32,
    ) -> PointerValue<'ctx> {
        // Basically just a helper function to do a gep then a load.
        let field = self
            .builder
            .build_struct_gep(
                self.context.get_struct_type(struct_type_name).unwrap(),
                ptr_at_struct,
                field_offset,
                "Field",
            )
            .unwrap();
        self.builder
            .build_load(self.ptr_ty, field, "Field value.")
            .unwrap()
            .into_pointer_value()
    }
    pub fn load_int_field_from_pointer_at_struct(
        &self,
        ptr_at_struct: PointerValue<'ctx>,
        struct_type_name: &str,
        int_type: IntType<'ctx>,
        field_offset: u32,
    ) -> IntValue<'ctx> {
        // Basically just a helper function to do a gep then a load.
        let field = self
            .builder
            .build_struct_gep(
                self.context.get_struct_type(struct_type_name).unwrap(),
                ptr_at_struct,
                field_offset,
                "Field",
            )
            .unwrap();
        self.builder
            .build_load(int_type, field, "Field value.")
            .unwrap()
            .into_int_value()
    }

    pub fn get_bool_for_value(
        &self,
        pred: IntValue,
        parent: Option<FunctionValue>,
    ) -> PointerValue<'ctx> {
        let then_val = self
            .module
            .get_global(&global_bool_ref(true))
            .unwrap()
            .as_pointer_value();

        let else_val = self
            .module
            .get_global(&global_bool_ref(false))
            .unwrap()
            .as_pointer_value();

        self.cond_builder(pred, || then_val, || else_val, self.ptr_ty.into(), parent)
            .into_pointer_value()
    }

    pub fn cond_builder<Ret: BasicValue<'ctx>, F1: Fn() -> Ret, F2: Fn() -> Ret>(
        &self,
        pred: IntValue,
        then_fn: F1,
        else_fn: F2,
        typ: BasicTypeEnum<'ctx>,
        parent: Option<FunctionValue>,
    ) -> BasicValueEnum<'ctx> {
        // Allows us to return the globals instead of allocating a new boolean each time.

        let p = match parent {
            None => self
                .module
                .get_function(self.current_fn.as_ref().unwrap())
                .unwrap(),
            Some(fn_val) => fn_val,
        };

        let then_bb = self.context.append_basic_block(p, "then");
        let else_bb = self.context.append_basic_block(p, "else");
        let cont_bb = self.context.append_basic_block(p, "ifcont");

        self.builder
            .build_conditional_branch(pred, then_bb, else_bb)
            .unwrap();

        // build then block
        self.builder.position_at_end(then_bb);
        let then_val = then_fn();

        self.builder.build_unconditional_branch(cont_bb).unwrap();

        let then_bb = self.builder.get_insert_block().unwrap();

        // build else block
        self.builder.position_at_end(else_bb);
        let else_val = else_fn();

        self.builder.build_unconditional_branch(cont_bb).unwrap();

        let else_bb = self.builder.get_insert_block().unwrap();

        // emit merge block
        self.builder.position_at_end(cont_bb);

        let phi = self.builder.build_phi(typ, "iftmp").unwrap();

        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

        let phi_basic = phi.as_basic_value();

        phi_basic
    }

    // Utils for Typcase

    pub fn code_is_member_of_set(
        &self,
        arg_type: IntValue<'ctx>,
        mut types: Vec<IntValue<'ctx>>,
    ) -> IntValue<'ctx> {
        if types.is_empty() {
            // Empty set, return false.
            return self.bool_ty.const_int(0, false);
        }
        // Else pop one element, compare, and recurse.
        let head = types.pop().unwrap();
        let lhs: IntValue = self
            .builder
            .build_int_compare::<IntValue>(IntPredicate::EQ, arg_type, head, "equal types")
            .unwrap();

        let rhs = self.code_is_member_of_set(arg_type, types);

        self.builder.build_or(lhs, rhs, "is_member").unwrap()
    }

    pub fn code_membership_predicate_for_types(
        &self,
        types: &[IntValue<'ctx>],
    ) -> FunctionValue<'ctx> {
        let fn_type = self.bool_ty.fn_type(&[self.i32_ty.into()], false);
        let fn_val = self.module.add_function(TYPE_IS_MEMBER, fn_type, None);
        fn_val
            .get_last_param()
            .unwrap()
            .into_int_value()
            .set_name("arg_type");

        let (_, _) = self.code_function_entry(TYPE_IS_MEMBER);

        let arg_type = fn_val.get_first_param().unwrap().into_int_value();

        let result = self.code_is_member_of_set(arg_type, types.to_vec());

        self.builder.build_return(Some(&result)).unwrap();

        fn_val
    }

    pub fn code_min_bound_finder_for_predicate(
        &self,
        membership_predicate: FunctionValue<'ctx>,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.i32_ty.fn_type(&[self.i32_ty.into()], false);
        let fn_val = self.module.add_function(MIN_BOUND_FINDER, fn_type, None);
        fn_val
            .get_last_param()
            .unwrap()
            .into_int_value()
            .set_name("arg_type");

        let (_, _) = self.code_function_entry(MIN_BOUND_FINDER);

        let arg_type = fn_val.get_first_param().unwrap().into_int_value();

        let types_contain_arg_type = self
            .builder
            .build_call(membership_predicate, &[arg_type.into()], "is_member")
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();

        let return_arg_type = || arg_type;
        let recurse = || {
            // Get parent type
            let parent_vector_pointer = self
                .module
                .get_global(PARENT_VECTOR)
                .unwrap()
                .as_pointer_value();
            let parent_vector_ty = self
                .i32_ty
                .vec_type(self.ct.class_id_order.len().try_into().unwrap());
            let parent_vector = self
                .builder
                .build_load(parent_vector_ty, parent_vector_pointer, "Parent Vector")
                .unwrap()
                .into_vector_value();
            let arg_type_parent = self
                .builder
                .build_extract_element(parent_vector, arg_type, "arg_type_parent")
                .unwrap();
            // Recursively call this function on parent
            self.builder
                .build_call(fn_val, &[arg_type_parent.into()], "recurse")
                .unwrap()
                .try_as_basic_value()
                .left()
                .unwrap()
                .into_int_value()
        };

        let result = self.cond_builder(
            types_contain_arg_type,
            return_arg_type,
            recurse,
            self.i32_ty.into(),
            Some(fn_val),
        );

        self.builder.build_return(Some(&result)).unwrap();

        fn_val
    }

    pub fn code_select_and_eval_case(
        &mut self,
        value_arg: PointerValue,
        type_arg: IntValue,
        mut cases: Cases,
    ) -> PointerValue<'ctx> {
        if cases.is_empty() {
            // Should never get here if program passes typechecking.
            return self.ptr_ty.const_null();
        }
        let case = cases.pop().unwrap();
        let case_type = self.sym_to_class_id_int_val(&case.typ);

        // Can't use cond_builder because closures are FnOnce.
        let parent = self
            .module
            .get_function(self.current_fn.as_ref().unwrap())
            .unwrap();

        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let cont_bb = self.context.append_basic_block(parent, "ifcont");

        let found_match: IntValue = self
            .builder
            .build_int_compare::<IntValue>(IntPredicate::EQ, type_arg, case_type, "equal types")
            .unwrap();

        self.builder
            .build_conditional_branch(found_match, then_bb, else_bb)
            .unwrap();

        // build then block
        self.builder.position_at_end(then_bb);
        self.variables.enter_scope();
        let case_alloca = self
            .builder
            .build_alloca(self.ptr_ty, "case binding alloca")
            .unwrap();
        self.builder.build_store(case_alloca, value_arg).unwrap();
        self.variables.add_binding(&case.id, &case_alloca);
        let then_val = self.codegen(&case.expr);
        self.variables.exit_scope();

        self.builder.build_unconditional_branch(cont_bb).unwrap();

        let then_bb = self.builder.get_insert_block().unwrap();

        // build else block
        self.builder.position_at_end(else_bb);
        let else_val = self.code_select_and_eval_case(value_arg, type_arg, cases);

        self.builder.build_unconditional_branch(cont_bb).unwrap();

        let else_bb = self.builder.get_insert_block().unwrap();

        // emit merge block
        self.builder.position_at_end(cont_bb);

        let phi = self.builder.build_phi(self.ptr_ty, "iftmp").unwrap();

        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

        let phi_basic = phi.as_basic_value();

        phi_basic.into_pointer_value()
    }
}
