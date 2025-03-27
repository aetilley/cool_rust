use crate::ast::Expr;
use crate::symbol::sym;
use inkwell::module::Linkage;

use crate::codegen::codegen_constants::*;
use crate::codegen::CodeGenManager;

use common_macros::hash_map;

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
            // char *fgets(char *str, int n, FILE *stream);
            "fgets" => self.ptr_ty.fn_type(&[self.ptr_ty.into(), self.i32_ty.into(), self.ptr_ty.into()], false),
            // FILE *fdopen(int fildes, const char *mode);
            "fdopen" => self.ptr_ty.fn_type(&[self.i32_ty.into(), self.ptr_ty.into()], false),
            // void *memcpy(void *to, const void *from, size_t numBytes);
            "memcpy" => self.context.void_type().fn_type(&[self.ptr_ty.into(), self.ptr_ty.into(), self.i32_ty.into()], false)
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

    fn code_io_in_string(&self) {
        let fn_name = method_ref(&sym(IO), &sym(IN_STRING));
        let (_fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let buff_size = self.i32_ty.const_int(MAX_IN_STRING_LEN, false);
        let array_type = self.i8_ty.array_type(MAX_IN_STRING_LEN.try_into().unwrap());
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

        let result = self.code_new_string_from_ptr(dest_ptr, array_type);

        self.builder.build_return(Some(&result)).unwrap();
    }

    fn code_io_in_int(&self) {
        let fn_name = method_ref(&sym(IO), &sym(IN_INT));
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

    fn code_string_substring(&self) {
        let fn_name = method_ref(&sym(STRING), &sym(SUBSTRING));
        let (fn_val, _entry_block) = self.code_function_entry(&fn_name);

        let original_string = fn_val.get_first_param().unwrap().into_pointer_value();
        let original_field = self
            .builder
            .build_struct_gep(
                self.cl_string_ty,
                original_string,
                STRING_CONTENT_IND,
                "gep",
            )
            .unwrap();

        let begin_int_ptr = fn_val.get_nth_param(1).unwrap().into_pointer_value();
        let begin = self.load_int_field_from_pointer_at_struct(
            begin_int_ptr,
            INT,
            self.i32_ty,
            INT_VAL_IND,
        );
        let length_int_ptr = fn_val.get_nth_param(2).unwrap().into_pointer_value();
        let length = self.load_int_field_from_pointer_at_struct(
            length_int_ptr,
            INT,
            self.i32_ty,
            INT_VAL_IND,
        );
        let src_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(self.i8_ty, original_field, &[begin], "index")
                .unwrap()
        };

        let buff_size = self.i32_ty.const_int(MAX_IN_STRING_LEN, false);
        let array_type = self.i8_ty.array_type(MAX_IN_STRING_LEN.try_into().unwrap());
        let dest_ptr = self
            .builder
            .build_array_malloc(self.i8_ty, buff_size, "buffer_ptr")
            .unwrap();
        // Call to memcpy
        self.builder
            .build_memcpy(dest_ptr, 8, src_ptr, 8, length)
            .unwrap();

        let result = self.code_new_string_from_ptr(dest_ptr, array_type);

        self.builder.build_return(Some(&result)).unwrap();
    }

    fn code_object_abort(&self) {
        let fn_name = method_ref(&sym(OBJECT), &sym(ABORT));
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

    fn code_object_copy(&self) {
        let fn_name = method_ref(&sym(OBJECT), &sym(COPY));
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

    fn code_object_type_name(&self) {
        let fn_name = method_ref(&sym(OBJECT), &sym(TYPE_NAME));
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

    pub fn code_native_method_bodies(&mut self) {
        self.declare_externals();

        // NI
        self.code_object_abort();
        // NI
        self.code_object_type_name();
        // NI
        self.code_object_copy();
        self.code_io_out_string();
        self.code_io_out_int();
        self.code_io_in_string();
        // NI
        self.code_io_in_int();
        self.code_string_length();
        // NI
        self.code_string_concat();
        // NI
        self.code_string_substring();
    }
}
