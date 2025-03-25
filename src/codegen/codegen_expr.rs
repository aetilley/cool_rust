// Expr Codegen

use crate::ast::{Expr, ExprData, Formal};
use crate::symbol::{sym, Sym};
use either::Either::Left;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue};
use inkwell::IntPredicate;

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

impl<'ctx> CodeGenManager<'ctx> {
    pub fn codegen(&mut self, expr: &Expr) -> PointerValue<'ctx> {
        let data = &*expr.data;
        match data {
            ExprData::New { typ } => self.code_new_and_init(typ),
            ExprData::Assign { id, expr } => {
                let new_val_ptr = self.codegen(expr);

                // First check variables
                if let Some(ptr_to_ptr) = self.variables.lookup(id) {
                    self.builder.build_store(ptr_to_ptr, new_val_ptr).unwrap();
                    return new_val_ptr;
                }

                // else check attributes
                let cls = self.current_class.clone().unwrap();
                let attrs: Vec<(Sym, Sym, Expr)> = self.ct.get_all_attrs(&cls);
                if let Some(offset) = attrs.iter().position(|(name, _, _)| name == id) {
                    let field_offset: u32 =
                        <usize as TryInto<u32>>::try_into(offset).unwrap() + OBJECT_PREFIX_SIZE;
                    let self_ptr_alloc = self.variables.lookup(&sym(SELF)).unwrap();
                    let self_ptr = self
                        .builder
                        .build_load(self.ptr_ty, self_ptr_alloc, "self_ptr")
                        .unwrap()
                        .into_pointer_value();
                    self.store_pointer_value_into_pointer_at_struct(
                        self_ptr,
                        &cls,
                        field_offset,
                        new_val_ptr,
                    );
                    return new_val_ptr;
                }

                panic!("No identifier {} in scope.", id);
            }
            ExprData::Object { id } => {
                // First check stack then in any class attributes
                if let Some(alloc_ptr) = self.variables.lookup(id) {
                    let ptr = self
                        .builder
                        .build_load(self.ptr_ty, alloc_ptr, "arg_ptr")
                        .unwrap()
                        .into_pointer_value();
                    return ptr;
                };
                // Otherwise check attrs

                let cls = self.current_class.clone().unwrap();
                let attrs: Vec<(Sym, Sym, Expr)> = self.ct.get_all_attrs(&cls);
                if let Some(offset) = attrs.iter().position(|(name, _, _)| name == id) {
                    let field_offset: u32 =
                        <usize as TryInto<u32>>::try_into(offset).unwrap() + OBJECT_PREFIX_SIZE;
                    let self_ptr_alloc = self.variables.lookup(&sym(SELF)).unwrap();
                    let self_ptr = self
                        .builder
                        .build_load(self.ptr_ty, self_ptr_alloc, "self_ptr")
                        .unwrap()
                        .into_pointer_value();
                    return self.load_pointer_field_from_pointer_at_struct(
                        self_ptr,
                        &cls,
                        field_offset,
                    );
                }
                panic!("No identifier {} in scope.", id);
            }
            ExprData::NoExpr {} => self.ptr_ty.const_null(),
            ExprData::StrConst { val } => {
                let global_name = global_string_ref(val);
                // We may want to catch cases where the global is not found and do a malloc.  For now we panic.
                self.module
                    .get_global(&global_name)
                    .unwrap_or_else(|| panic!("No static string found for '{}'", val))
                    .as_pointer_value()
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
            ExprData::Not { expr } => {
                let struct_ptr = self.codegen(expr);
                let val = self.load_int_field_from_pointer_at_struct(
                    struct_ptr,
                    BOOL,
                    self.bool_ty,
                    BOOL_VAL_IND,
                );
                let one = self.bool_ty.const_int(1, false);
                let negation = self.builder.build_xor(val, one, "negation").unwrap();
                self.code_new_int(negation)
            }
            ExprData::IsVoid { expr } => {
                let val = self.codegen(expr);
                let is_null = self.builder.build_is_null(val, "isnull").unwrap();
                self.get_bool_for_value(is_null)
            }

            ExprData::Plus { lhs, rhs }
            | ExprData::Minus { lhs, rhs }
            | ExprData::Times { lhs, rhs }
            | ExprData::Divide { lhs, rhs } => {
                let lhs_ptr = self.codegen(lhs);
                let rhs_ptr = self.codegen(rhs);
                let l_int_value = self.load_int_field_from_pointer_at_struct(
                    lhs_ptr,
                    INT,
                    self.i32_ty,
                    INT_VAL_IND,
                );
                let r_int_value = self.load_int_field_from_pointer_at_struct(
                    rhs_ptr,
                    INT,
                    self.i32_ty,
                    INT_VAL_IND,
                );
                let value = match data {
                    ExprData::Plus { lhs: _, rhs: _ } => {
                        self.builder.build_int_add(l_int_value, r_int_value, "sum")
                    }
                    ExprData::Minus { lhs: _, rhs: _ } => {
                        self.builder
                            .build_int_sub(l_int_value, r_int_value, "difference")
                    }
                    ExprData::Times { lhs: _, rhs: _ } => {
                        self.builder
                            .build_int_mul(l_int_value, r_int_value, "product")
                    }
                    ExprData::Divide { lhs: _, rhs: _ } => {
                        self.builder
                            .build_int_signed_div(l_int_value, r_int_value, "quotient")
                    }
                    _ => panic!(""),
                };

                self.code_new_int(value.unwrap())
            }
            ExprData::Lt { lhs, rhs } | ExprData::Leq { lhs, rhs } => {
                let comp_op = match data {
                    ExprData::Lt { lhs: _, rhs: _ } => IntPredicate::ULT,
                    ExprData::Leq { lhs: _, rhs: _ } => IntPredicate::ULE,
                    _ => {
                        panic!();
                    }
                };
                let lhs_ptr = self.codegen(lhs);
                let rhs_ptr = self.codegen(rhs);
                let l_int_value = self.load_int_field_from_pointer_at_struct(
                    lhs_ptr,
                    INT,
                    self.i32_ty,
                    INT_VAL_IND,
                );
                let r_int_value = self.load_int_field_from_pointer_at_struct(
                    rhs_ptr,
                    INT,
                    self.i32_ty,
                    INT_VAL_IND,
                );

                let does_compare: IntValue = self
                    .builder
                    .build_int_compare::<IntValue>(comp_op, l_int_value, r_int_value, "ifcond")
                    .unwrap();

                self.get_bool_for_value(does_compare)
            }
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
                    let l_int_value = self.load_int_field_from_pointer_at_struct(
                        lhs_ptr,
                        stype,
                        self.i32_ty,
                        field_ind,
                    );
                    let r_int_value = self.load_int_field_from_pointer_at_struct(
                        rhs_ptr,
                        stype,
                        self.i32_ty,
                        field_ind,
                    );

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
                            self.cl_string_ty,
                            lhs_ptr,
                            STRING_CONTENT_IND,
                            "left string array field",
                        )
                        .unwrap();
                    let r_array_field = self
                        .builder
                        .build_struct_gep(
                            self.cl_string_ty,
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
                            self.i32_ty.const_zero(),
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
            ExprData::Block { exprs } => {
                let mut result: PointerValue = self.ptr_ty.const_null();
                for expr in exprs.iter() {
                    result = self.codegen(expr);
                }
                result
            }
            ExprData::Cond {
                pred,
                then_expr,
                else_expr,
            } => {
                let bool_struct_ptr = self.codegen(pred);
                let pred_val = self.load_int_field_from_pointer_at_struct(
                    bool_struct_ptr,
                    BOOL,
                    self.bool_ty,
                    BOOL_VAL_IND,
                );
                // Note that we can't call the cond_builder utility because the two closure argument would have to be
                // FnMut (and we can't have two of those closing around self at the same time).

                let one_const = self.bool_ty.const_int(1, false);

                let pred: IntValue = self
                    .builder
                    .build_int_compare::<IntValue>(IntPredicate::EQ, pred_val, one_const, "ifcond")
                    .unwrap();

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
                let then_val = self.codegen(then_expr);

                self.builder.build_unconditional_branch(cont_bb).unwrap();

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.codegen(else_expr);

                self.builder.build_unconditional_branch(cont_bb).unwrap();

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);

                let phi = self.builder.build_phi(self.ptr_ty, "iftmp").unwrap();

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                let phi_basic = phi.as_basic_value();

                phi_basic.into_pointer_value()
            }
            ExprData::TypCase { expr, cases: _ } => {
                let _val_struct_ptr = self.codegen(expr);

                todo!()
            }
            ExprData::Let {
                id,
                typ: _,
                init,
                body,
            } => {
                let val = self.codegen(init);
                self.variables.enter_scope();
                let val_alloc = self
                    .builder
                    .build_alloca(self.ptr_ty, "let alloca")
                    .unwrap();
                self.builder.build_store(val_alloc, val).unwrap();
                self.variables.add_binding(id, &val_alloc);
                let result = self.codegen(body);
                self.variables.exit_scope();
                result
            }
            ExprData::Loop { pred, body } => {
                let parent = self
                    .module
                    .get_function(self.current_fn.as_ref().unwrap())
                    .unwrap();

                let pred_block = self.context.append_basic_block(parent, "pred");
                let body_block = self.context.append_basic_block(parent, "body");
                let exit_block = self.context.append_basic_block(parent, "exit");

                self.builder.build_unconditional_branch(pred_block).unwrap();

                self.builder.position_at_end(pred_block);
                let pred_struct_ptr = self.codegen(pred);
                let pred_val = self.load_int_field_from_pointer_at_struct(
                    pred_struct_ptr,
                    BOOL,
                    self.bool_ty,
                    BOOL_VAL_IND,
                );

                self.builder
                    .build_conditional_branch(pred_val, body_block, exit_block)
                    .unwrap();

                self.builder.position_at_end(body_block);
                let _ = self.codegen(body);
                self.builder.build_unconditional_branch(pred_block).unwrap();

                self.builder.position_at_end(exit_block);
                self.ptr_ty.const_null()
            }

            ExprData::Dispatch {
                slf,
                method_name,
                args,
            } => {
                let slf_arg = self.codegen(slf);

                // Note we are guaranteed that the initial segment
                // up to CLASS_ID_IND will be the same layout
                // as that of "Object".
                let class_id_field = self.load_int_field_from_pointer_at_struct(
                    slf_arg,
                    &sym("Object"),
                    self.i32_ty,
                    CLASS_ID_IND,
                );

                let vtable_master_pointer = self
                    .module
                    .get_global(VTABLE_MASTER_VECTOR)
                    .unwrap()
                    .as_pointer_value();

                let vtable_master_ty = self
                    .ptr_ty
                    .vec_type(self.ct.class_id_order.len().try_into().unwrap());
                let vtable_master_vector = self
                    .builder
                    .build_load(vtable_master_ty, vtable_master_pointer, "VtableMaster")
                    .unwrap()
                    .into_vector_value();

                let vtable_pointer = self
                    .builder
                    .build_extract_element(vtable_master_vector, class_id_field, "vtable pointer")
                    .unwrap()
                    .into_pointer_value();

                let mut static_type = &slf.stype;
                if static_type == &sym("SELF_TYPE") {
                    static_type = match &self.current_class {
                        Some(cls) => cls,
                        _ => {
                            panic!("");
                        }
                    }
                }
                // Using the static type is ok here because the offset of the method is the same in
                // all types that that inherit from it.
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

                let vtable_type = self
                    .ptr_ty
                    .array_type(self.ct.get_max_vtable_size().try_into().unwrap());

                let vtable_array = self
                    .builder
                    .build_load(vtable_type, vtable_pointer, "load vtable")
                    .unwrap()
                    .into_array_value();

                let method_pointer = self
                    .builder
                    .build_extract_value(vtable_array, vtable_offset, "method pointer")
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

                let mut compiled_args: Vec<BasicMetadataValueEnum> = vec![slf_arg.into()];
                for arg in args.iter() {
                    let compiled_arg = self.codegen(arg);
                    compiled_args.push(compiled_arg.into());
                }

                let call_name = &format!("{}_call", fn_name);

                // Call the function.
                let result = self
                    .builder
                    .build_indirect_call(fn_type, method_pointer, &compiled_args, call_name)
                    .unwrap()
                    .try_as_basic_value();

                match result {
                    Left(BasicValueEnum::PointerValue(ptr)) => ptr,
                    _ => self.ptr_ty.const_null(),
                }
            }

            ExprData::Comp { expr } => {
                // Integer complement (flip every bit)
                let val = self.codegen(expr);
                let inner =
                    self.load_int_field_from_pointer_at_struct(val, INT, self.i32_ty, INT_VAL_IND);
                let inverted = self.builder.build_not(inner, "inverted").unwrap();
                self.code_new_int(inverted)
            }

            ExprData::StaticDispatch {
                typ,
                method_name,
                args,
                slf,
            } => {
                let slf_arg = self.codegen(slf);
                let mut compiled_args: Vec<BasicMetadataValueEnum> = vec![slf_arg.into()];
                for arg in args.iter() {
                    let compiled_arg = self.codegen(arg);
                    compiled_args.push(compiled_arg.into());
                }
                let fn_name = &method_ref(typ, method_name);
                let fn_val = self.module.get_function(fn_name).unwrap();
                let call_name = &format!("{}_call", fn_name);

                let result = self
                    .builder
                    .build_call(fn_val, &compiled_args[..], call_name)
                    .unwrap()
                    .try_as_basic_value();
                match result {
                    Left(BasicValueEnum::PointerValue(ptr)) => ptr,
                    _ => self.ptr_ty.const_null(),
                }
            }
        }
    }
}
