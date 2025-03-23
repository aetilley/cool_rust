// Expr Codegen

use crate::ast::{Expr, ExprData, Formal};
use crate::symbol::{sym, Sym};
use either::Either::Left;
use inkwell::types::IntType;
use inkwell::values::{ArrayValue, BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue};
use inkwell::IntPredicate;

use crate::codegen::CodeGenManager;
use crate::codegen_constants::*;

impl<'ctx> CodeGenManager<'ctx> {
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

    fn malloc_new_bool_with_value(&self, value: IntValue) -> PointerValue<'ctx> {
        let new_ptr = self.code_new_and_init(&sym("Bool"));

        let field = self
            .builder
            .build_struct_gep(
                self.context.get_struct_type("Bool").unwrap(),
                new_ptr,
                BOOL_VAL_IND,
                "gep",
            )
            .unwrap();
        self.builder.build_store(field, value).unwrap();

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
        let self_arg = BasicMetadataValueEnum::PointerValue(new_ptr);
        let call_name = &format!("{}_init_call", typ);
        self.builder
            .build_call(initializer, &[self_arg], call_name)
            .unwrap();
        new_ptr
    }

    fn load_int_field_from_pointer_at_struct(
        &self,
        ptr: PointerValue<'ctx>,
        struct_type_name: &str,
        int_type: IntType<'ctx>,
        field_offset: u32,
    ) -> BasicValueEnum<'ctx> {
        // Basically just a helper function to do a gep then a load.
        let field = self
            .builder
            .build_struct_gep(
                self.context.get_struct_type(struct_type_name).unwrap(),
                ptr,
                field_offset,
                "Field",
            )
            .unwrap();
        self.builder
            .build_load(int_type, field, "Field value.")
            .unwrap()
    }

    // fn load_array_field_from_pointer_at_struct(
    //     &self,
    //     ptr: PointerValue<'ctx>,
    //     struct_type_name: &str,
    //     array_type: ArrayType<'ctx>,
    //     field_offset: u32,
    // ) -> BasicValueEnum<'ctx> {
    //     // Basically just a helper function to do a gep then a load.
    //     let field = self
    //         .builder
    //         .build_struct_gep(
    //             self.context.get_struct_type(struct_type_name).unwrap(),
    //             ptr,
    //             field_offset,
    //             "Field",
    //         )
    //         .unwrap();
    //     self.builder
    //         .build_load(array_type, field, "Field value.")
    //         .unwrap()
    // }

    fn get_bool_for_value(&self, pred: IntValue) -> PointerValue<'ctx> {
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

        self.cond_builder(pred, || then_val, || else_val)
    }

    fn cond_builder<F1: Fn() -> PointerValue<'ctx>, F2: Fn() -> PointerValue<'ctx>>(
        &self,
        pred: IntValue,
        then_fn: F1,
        else_fn: F2,
    ) -> PointerValue<'ctx> {
        // Allows us to return the globals instead of allocating a new boolean each time.

        let parent = self
            .module
            .get_function(self.current_fn.as_ref().unwrap())
            .unwrap();

        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let cont_bb = self.context.append_basic_block(parent, "ifcont");

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

        let phi = self
            .builder
            .build_phi(self.context.ptr_type(self.aspace), "iftmp")
            .unwrap();

        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

        let phi_basic = phi.as_basic_value();

        phi_basic.into_pointer_value()
    }

    pub fn codegen(&self, expr: &Expr) -> PointerValue<'ctx> {
        let data = &*expr.data;
        match data {
            ExprData::NoExpr {} => self.context.ptr_type(self.aspace).const_null(),
            ExprData::Eq { lhs, rhs } => {
                let lhs_ptr = self.codegen(lhs);
                let rhs_ptr = self.codegen(rhs);
                let stype = &lhs.stype[..];
                if stype == INT || stype == BOOL {
                    let field_ind = if stype == INT {
                        INT_VAL_IND
                    } else {
                        BOOL_VAL_IND
                    };
                    let l_int_value = self
                        .load_int_field_from_pointer_at_struct(
                            lhs_ptr,
                            stype,
                            self.context.i32_type(),
                            field_ind,
                        )
                        .into_int_value();
                    let r_int_value = self
                        .load_int_field_from_pointer_at_struct(
                            rhs_ptr,
                            stype,
                            self.context.i32_type(),
                            field_ind,
                        )
                        .into_int_value();

                    let are_equal: IntValue = self
                        .builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::EQ,
                            l_int_value,
                            r_int_value,
                            "ifcond",
                        )
                        .unwrap();

                    self.get_bool_for_value(are_equal)
                } else if stype == STRING {
                    let l_array_field = self
                        .builder
                        .build_struct_gep(
                            self.context.get_struct_type(STRING).unwrap(),
                            lhs_ptr,
                            STRING_CONTENT_IND,
                            "left string array field",
                        )
                        .unwrap();
                    let r_array_field = self
                        .builder
                        .build_struct_gep(
                            self.context.get_struct_type(STRING).unwrap(),
                            rhs_ptr,
                            STRING_CONTENT_IND,
                            "right string array field",
                        )
                        .unwrap();

                    let strcmp_return = self
                        .builder
                        .build_call(
                            self.module
                                .get_function("strcmp")
                                .expect("No function named \"strcmp\" "),
                            &[l_array_field.into(), r_array_field.into()],
                            "are_equal_i32",
                        )
                        .unwrap()
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_int_value();

                    let are_equal: IntValue = self
                        .builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::EQ,
                            strcmp_return,
                            self.context.i32_type().const_zero(),
                            "are_equal_i1",
                        )
                        .unwrap();

                    self.get_bool_for_value(are_equal)
                } else {
                    let lhs_to_int = self
                        .builder
                        .build_ptr_to_int(lhs_ptr, self.context.i64_type(), "lhs_ptr to int")
                        .unwrap();
                    let rhs_to_int = self
                        .builder
                        .build_ptr_to_int(rhs_ptr, self.context.i64_type(), "rhs_ptr to int")
                        .unwrap();

                    // Default to pointer equality.
                    let are_equal: IntValue = self
                        .builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::EQ,
                            lhs_to_int,
                            rhs_to_int,
                            "pointers_are_equal",
                        )
                        .unwrap();

                    self.get_bool_for_value(are_equal)
                }
            }
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
                    .unwrap_or_else(|| panic!("No static int found for '{}'", val))
                    .as_pointer_value()
            }
            ExprData::BoolConst { val } => {
                let global_name = global_bool_ref(*val);
                self.module
                    .get_global(&global_name)
                    .unwrap_or_else(|| panic!("No static bool found for '{}'", val))
                    .as_pointer_value()
            }
            ExprData::Cond {
                pred,
                then_expr,
                else_expr,
            } => {
                let bool_struct_ptr = self.codegen(pred);
                let pred_val = self
                    .load_int_field_from_pointer_at_struct(
                        bool_struct_ptr,
                        BOOL,
                        self.context.bool_type(),
                        BOOL_VAL_IND,
                    )
                    .into_int_value();

                let one_const = self.context.bool_type().const_int(1, false);

                let pred: IntValue = self
                    .builder
                    .build_int_compare::<IntValue>(IntPredicate::EQ, pred_val, one_const, "ifcond")
                    .unwrap();

                let then_fn = || self.codegen(then_expr);
                let else_fn = || self.codegen(else_expr);
                let phi_ptr = self.cond_builder(pred, then_fn, else_fn);
                phi_ptr
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
                let mut static_type = &slf.stype;
                if static_type == &sym("SELF_TYPE") {
                    static_type = match &self.current_class {
                        Some(cls) => cls,
                        _ => {
                            panic!("");
                        }
                    }
                }

                let vtable_offset: u32 = self
                    .ct
                    .class_method_order
                    .get(static_type)
                    .unwrap_or_else(|| panic!("No methods found for type {}", static_type))
                    .iter()
                    .position(|r| r == method_name)
                    .unwrap()
                    .try_into()
                    .unwrap();

                // Note we are guaranteed that the initial segment
                // of the struct will be the layout of static_type.
                // In fact we only need the first field which is the same in all
                // classes.
                let pointee_ty = self.context.get_struct_type(static_type).unwrap();
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
                    self.ct.get_method(static_type, method_name).unwrap();

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
