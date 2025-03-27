use crate::ast::Expr;
use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;
use crate::symbol::sym;

use common_macros::hash_map;
use inkwell::module::Linkage;
use inkwell::values::PointerValue;

impl CodeGenManager<'_> {
    // External functions.

    pub fn declare_externals(&self) {
        let externals_types = hash_map! {
            // int strcmp(const char *s1, const char *s2);
            "strcmp" => self.i32_ty.fn_type(&[self.ptr_ty.into(), self.ptr_ty.into()], false),
            // size_t strlen(const char* str)
            "strlen" => self.i32_ty.fn_type(&[self.ptr_ty.into()], false),
            // int printf (const char * format, ...);
            "printf" => self.i32_ty.fn_type(&[self.ptr_ty.into()], true),
            // char* fgets(char *str, int n, FILE *stream);
            "fgets" => self.ptr_ty.fn_type(&[self.ptr_ty.into(), self.i32_ty.into(), self.ptr_ty.into()], false),
            // FILE* fdopen(int fildes, const char *mode);
            "fdopen" => self.ptr_ty.fn_type(&[self.i32_ty.into(), self.ptr_ty.into()], false),
            // long int strtol(const char *nptr, char **endptr, int base);
            "strtol" => self.context.i64_type().fn_type(&[self.ptr_ty.into(), self.ptr_ty.into(), self.i32_ty.into()], false),
            // char* strcpy( char* dest, const char* src );
            "strcpy" => self.ptr_ty.fn_type(&[self.ptr_ty.into(), self.ptr_ty.into()], false),
            // char* strcat( char *dest, const char *src );
            "strcat" => self.ptr_ty.fn_type(&[self.ptr_ty.into(), self.ptr_ty.into()], false),
            // void *memcpy(void *to, const void *from, size_t numBytes);
            "memcpy" => self.context.void_type().fn_type(&[self.ptr_ty.into(), self.ptr_ty.into(), self.i32_ty.into()], false),
            // void abort()
            "abort" => self.context.void_type().fn_type(&[], false),
            // void *malloc( size_t size );
            "malloc" => self.context.void_type().fn_type(&[self.i32_ty.into()], false),


        };

        for (fn_name, fn_type) in externals_types.iter() {
            self.module
                .add_function(fn_name, fn_type.to_owned(), Some(Linkage::External));
        }
    }

    // Native Class Methods

    fn code_string_length(&self) {
        let fn_name = method_ref(&sym(STRING), &sym(LENGTH));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);
        let self_ptr = fn_val.get_first_param().unwrap().into_pointer_value();

        let value =
            self.load_pointer_field_from_pointer_at_struct(self_ptr, STRING, STRING_LEN_IND);

        self.builder.build_return(Some(&value)).unwrap();
        fn_val.verify(false);
    }

    fn code_io_out_int(&mut self) {
        let fn_name = method_ref(&sym(IO), &sym(OUT_INT));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let format_string_array = self.code_array_value_from_sym(&sym("%d\n\0"));
        let format_string_ptr = self
            .builder
            .build_alloca(format_string_array.get_type(), "ptr to fmt string")
            .unwrap();

        self.builder
            .build_store(format_string_ptr, format_string_array)
            .unwrap();

        let arg = fn_val.get_last_param().unwrap().into_pointer_value();
        // Get String array from second field of *String
        let to_print_field =
            self.load_int_field_from_pointer_at_struct(arg, INT, self.i32_ty, INT_VAL_IND);

        let printf = self.module.get_function("printf").unwrap();
        let _call = self
            .builder
            .build_call(
                printf,
                &[format_string_ptr.into(), to_print_field.into()],
                "call_printf",
            )
            .unwrap();

        let body_val = self.codegen(&Expr::new(&sym("Object")));
        self.builder.build_return(Some(&body_val)).unwrap();
        fn_val.verify(false);
    }

    fn code_io_out_string(&mut self) {
        let fn_name = method_ref(&sym(IO), &sym(OUT_STRING));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let arg = fn_val.get_last_param().unwrap().into_pointer_value();
        // Get String array from second field of *String
        let to_print_field = self
            .builder
            .build_struct_gep(self.cl_string_ty, arg, STRING_CONTENT_IND, "gep")
            .unwrap();

        let format_string_array = self.code_array_value_from_sym(&sym("%s\n\0"));

        let format_string_ptr = self
            .builder
            .build_alloca(format_string_array.get_type(), "ptr to fmt string")
            .unwrap();

        self.builder
            .build_store(format_string_ptr, format_string_array)
            .unwrap();

        let printf = self.module.get_function("printf").unwrap();
        let _call = self
            .builder
            .build_call(
                printf,
                &[format_string_ptr.into(), to_print_field.into()],
                "call_printf",
            )
            .unwrap();

        let body_val = self.codegen(&Expr::new(&sym("Object")));
        self.builder.build_return(Some(&body_val)).unwrap();
        fn_val.verify(false);
    }

    fn code_read_stdin_to_ptr(&self) -> PointerValue {
        // Common code used in both String.in_ methods.

        let buff_size = self.i32_ty.const_int(MAX_IN_STRING_LEN, false);
        let dest_ptr = self
            .builder
            .build_array_malloc(self.i8_ty, buff_size, "buffer_ptr")
            .unwrap();
        // use fdopen to get stdin
        // FILE* fdopen (int fildes, const char *mode)
        let zero = self.i32_ty.const_zero();
        let mode_array = self.code_array_value_from_sym(&sym("r"));
        let mode_ptr = self
            .builder
            .build_alloca(mode_array.get_type(), "mode")
            .unwrap();
        self.builder.build_store(mode_ptr, mode_array).unwrap();
        let stdin = self
            .builder
            .build_call(
                self.module.get_function("fdopen").unwrap(),
                &[zero.into(), mode_ptr.into()],
                "_",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap();

        // fgets(char*, int, stdin)
        let max = self.i32_ty.const_int(MAX_IN_STRING_LEN, false);
        let _ = self
            .builder
            .build_call(
                self.module.get_function("fgets").unwrap(),
                &[dest_ptr.into(), max.into(), stdin.into()],
                "_",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap();
        dest_ptr
    }

    fn code_io_in_string(&self) {
        let fn_name = method_ref(&sym(IO), &sym(IN_STRING));
        let (_fn_val, _entry_block) = self.code_function_entry(&fn_name);
        let array_type = self.i8_ty.array_type(MAX_IN_STRING_LEN.try_into().unwrap());

        let input = self.code_read_stdin_to_ptr();

        let result = self.code_new_string_from_ptr(input, array_type);

        self.builder.build_return(Some(&result)).unwrap();
    }

    fn code_io_in_int(&self) {
        let fn_name = method_ref(&sym(IO), &sym(IN_INT));
        let (_fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let input = self.code_read_stdin_to_ptr();

        // convert to int
        let null = self.ptr_ty.const_null();
        let ten = self.i32_ty.const_int(10, false);
        let result_long = self
            .builder
            .build_call(
                self.module.get_function("strtol").unwrap(),
                &[input.into(), null.into(), ten.into()],
                "as_int",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        let result = self
            .builder
            .build_int_truncate(result_long, self.i32_ty, "truncate")
            .unwrap();

        let result = self.code_new_int(result);

        self.builder.build_return(Some(&result)).unwrap();
    }

    fn code_string_concat(&self) {
        let fn_name = method_ref(&sym(STRING), &sym(CONCAT));
        let (_fn_val, _entry_block) = self.code_function_entry(&fn_name);

        // Warning message
        let format_string_array = self.code_array_value_from_sym(&sym("%s\n\0"));

        let format_string_ptr = self
            .builder
            .build_alloca(format_string_array.get_type(), "ptr to fmt string")
            .unwrap();

        self.builder
            .build_store(format_string_ptr, format_string_array)
            .unwrap();

        let warning_array =
            self.code_array_value_from_sym(&sym(&format!("{} is not yet implemented\0", fn_name)));
        let warning_ptr = self
            .builder
            .build_alloca(warning_array.get_type(), "ptr to warning")
            .unwrap();
        self.builder
            .build_store(warning_ptr, warning_array)
            .unwrap();

        let printf = self.module.get_function("printf").unwrap();
        let _call = self
            .builder
            .build_call(
                printf,
                &[format_string_ptr.into(), warning_ptr.into()],
                "call_printf",
            )
            .unwrap();

        let result = self.ptr_ty.const_null();
        self.builder.build_return(Some(&result)).unwrap();
    }

    // fn code_string_concat(&mut self) {
    //     let fn_name = method_ref(&sym(STRING), &sym(CONCAT));
    //     let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

    //     let slf = fn_val.get_first_param().unwrap().into_pointer_value();
    //     let arg = fn_val.get_last_param().unwrap().into_pointer_value();
    //     // Get String array from second field of *String
    //     let slf_content_ptr = self
    //         .builder
    //         .build_struct_gep(self.cl_string_ty, slf, STRING_CONTENT_IND, "gep")
    //         .unwrap();
    //     let arg_content_ptr = self
    //         .builder
    //         .build_struct_gep(self.cl_string_ty, arg, STRING_CONTENT_IND, "gep")
    //         .unwrap();

    //     // let slf_len =
    //     //     self.load_int_field_from_pointer_at_struct(slf, STRING, self.i32_ty, STRING_LEN_IND);
    //     // let arg_len =
    //     //     self.load_int_field_from_pointer_at_struct(arg, STRING, self.i32_ty, STRING_LEN_IND);

    //     let reserved = self.i32_ty.const_int(MAX_IN_STRING_LEN, false);
    //     let new_string = self
    //         .builder
    //         .build_array_malloc(self.i8_ty, reserved, "new_str")
    //         .unwrap();

    //     self.builder
    //         .build_call(
    //             self.module.get_function("strcpy").unwrap(),
    //             &[new_string.into(), slf_content_ptr.into()],
    //             "strcpy",
    //         )
    //         .unwrap();

    //     self.builder
    //         .build_call(
    //             self.module.get_function("strcat").unwrap(),
    //             &[new_string.into(), arg_content_ptr.into()],
    //             "strcpy",
    //         )
    //         .unwrap();

    //     let array_type = self.i8_ty.array_type(MAX_IN_STRING_LEN.try_into().unwrap());
    //     let result = self.code_new_string_from_ptr(new_string, array_type);
    //     self.builder.build_return(Some(&result)).unwrap();
    //     fn_val.verify(false);
    // }

    fn code_string_substring(&mut self) {
        let fn_name = method_ref(&sym(STRING), &sym(SUBSTRING));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);
        let body_val = self.codegen(&Expr::new(&sym("Object")));
        self.builder.build_return(Some(&body_val)).unwrap();
        fn_val.verify(false);
    }

    // fn code_string_substring(&self) {
    //     let fn_name = method_ref(&sym(STRING), &sym(SUBSTRING));
    //     let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

    //     let original_string = fn_val.get_first_param().unwrap().into_pointer_value();
    //     let original_field = self
    //         .builder
    //         .build_struct_gep(
    //             self.cl_string_ty,
    //             original_string,
    //             STRING_CONTENT_IND,
    //             "gep",
    //         )
    //         .unwrap();

    //     let begin_int_ptr = fn_val.get_nth_param(1).unwrap().into_pointer_value();
    //     let begin = self.load_int_field_from_pointer_at_struct(
    //         begin_int_ptr,
    //         INT,
    //         self.i32_ty,
    //         INT_VAL_IND,
    //     );
    //     let length_int_ptr = fn_val.get_nth_param(2).unwrap().into_pointer_value();
    //     let length = self.load_int_field_from_pointer_at_struct(
    //         length_int_ptr,
    //         INT,
    //         self.i32_ty,
    //         INT_VAL_IND,
    //     );
    //     let src_ptr = unsafe {
    //         self.builder
    //             .build_in_bounds_gep(self.i8_ty, original_field, &[begin], "index")
    //             .unwrap()
    //     };

    //     let buff_size = self.i32_ty.const_int(MAX_IN_STRING_LEN, false);
    //     let array_type = self.i8_ty.array_type(MAX_IN_STRING_LEN.try_into().unwrap());
    //     let dest_ptr = self
    //         .builder
    //         .build_array_malloc(self.i8_ty, buff_size, "buffer_ptr")
    //         .unwrap();
    //     // Call to memcpy
    //     // self.builder
    //     //     .build_memcpy(dest_ptr, 64, src_ptr, 64, length)
    //     //     .unwrap();
    //     self.builder
    //         .build_call(
    //             self.module.get_function("memcpy").unwrap(),
    //             &[dest_ptr.into(), src_ptr.into(), length.into()],
    //             "call memcpy",
    //         )
    //         .unwrap();

    //     let result = self.code_new_string_from_ptr(dest_ptr, array_type);

    //     self.builder.build_return(Some(&result)).unwrap();
    // }

    fn code_object_abort(&self) {
        let fn_name = method_ref(&sym(OBJECT), &sym(ABORT));
        let (_fn_val, _entry_block) = self.code_function_entry(&fn_name);

        self.builder
            .build_call(self.module.get_function("abort").unwrap(), &[], "abort")
            .unwrap();

        let result = self.code_new_and_init(&sym(OBJECT));
        self.builder.build_return(Some(&result)).unwrap();
    }

    fn code_object_copy(&self) {
        let fn_name = method_ref(&sym(OBJECT), &sym(COPY));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let self_ptr = fn_val.get_first_param().unwrap().into_pointer_value();

        // Get size of self.

        let size_table_ty = self
            .i32_ty
            .vec_type(self.ct.class_id_order.len().try_into().unwrap());
        let size_table_ptr = self
            .module
            .get_global(STRUCT_SIZE_TABLE)
            .unwrap()
            .as_pointer_value();
        let size_table = self
            .builder
            .build_load(size_table_ty, size_table_ptr, "size table")
            .unwrap();

        let class_id =
            self.load_int_field_from_pointer_at_struct(self_ptr, OBJECT, self.i32_ty, CLASS_ID_IND);

        let size = self
            .builder
            .build_extract_element(size_table.into_vector_value(), class_id, "struct_size")
            .unwrap()
            .into_int_value();

        let new_space = self
            .builder
            .build_call(
                self.module.get_function("malloc").unwrap(),
                &[size.into()],
                "malloc",
            )
            .unwrap()
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        self.builder
            .build_memcpy(new_space, 8, self_ptr, 8, size)
            .unwrap();

        self.builder.build_return(Some(&self_ptr)).unwrap();
    }

    fn code_object_type_name(&self) {
        let fn_name = method_ref(&sym(OBJECT), &sym(TYPE_NAME));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let self_ptr = fn_val.get_first_param().unwrap().into_pointer_value();

        let class_id =
            self.load_int_field_from_pointer_at_struct(self_ptr, OBJECT, self.i32_ty, CLASS_ID_IND);

        let type_name_vec_ptr = self
            .module
            .get_global(TYPE_NAME_VECTOR)
            .unwrap()
            .as_pointer_value();
        let type_name_vec_ty = self
            .ptr_ty
            .vec_type(self.ct.class_id_order.len().try_into().unwrap());
        let type_name_vec = self
            .builder
            .build_load(type_name_vec_ty, type_name_vec_ptr, "type_name_vector")
            .unwrap()
            .into_vector_value();
        let result = self
            .builder
            .build_extract_element(type_name_vec, class_id, "type name")
            .unwrap()
            .into_pointer_value();

        self.builder.build_return(Some(&result)).unwrap();
    }

    pub fn code_native_method_bodies(&mut self) {
        self.declare_externals();

        self.code_object_abort();
        self.code_object_type_name();
        // NI
        self.code_object_copy();
        self.code_io_out_string();
        self.code_io_out_int();
        self.code_io_in_string();
        self.code_io_in_int();
        self.code_string_length();
        // NI
        self.code_string_concat();
        // NI
        self.code_string_substring();
    }
}
