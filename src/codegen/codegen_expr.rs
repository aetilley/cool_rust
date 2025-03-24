// Expr Codegen

use crate::ast::{Expr, ExprData, Formal};
use crate::symbol::{sym, Sym};
use either::Either::Left;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, IntValue, PointerValue};
use inkwell::IntPredicate;

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

impl<'ctx> CodeGenManager<'ctx> {
    pub fn codegen(&self, expr: &Expr) -> PointerValue<'ctx> {
        let data = &*expr.data;
        match data {
            ExprData::NoExpr {} => self.context.ptr_type(self.aspace).const_null(),
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
                        .build_load(
                            self.context.ptr_type(self.aspace),
                            self_ptr_alloc,
                            "self_ptr",
                        )
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
                    self.context.i32_type(),
                    INT_VAL_IND,
                );
                let r_int_value = self.load_int_field_from_pointer_at_struct(
                    rhs_ptr,
                    INT,
                    self.context.i32_type(),
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
                        self.context.i32_type(),
                        field_ind,
                    );
                    let r_int_value = self.load_int_field_from_pointer_at_struct(
                        rhs_ptr,
                        stype,
                        self.context.i32_type(),
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
                // Otherwise check attrs

                let cls = self.current_class.clone().unwrap();
                let attrs: Vec<(Sym, Sym, Expr)> = self.ct.get_all_attrs(&cls);
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
                    return self.load_pointer_field_from_pointer_at_struct(
                        self_ptr,
                        &cls,
                        field_offset,
                    );
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
                let pred_val = self.load_int_field_from_pointer_at_struct(
                    bool_struct_ptr,
                    BOOL,
                    self.context.bool_type(),
                    BOOL_VAL_IND,
                );

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
