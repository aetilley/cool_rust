use crate::symbol::Sym;
use inkwell::types::BasicTypeEnum;

use crate::codegen::CodeGenManager;

// Declare and Code Class Structs
impl CodeGenManager<'_> {
    fn declare_native_class_struct_types(&self) {
        self.context.opaque_struct_type("Object");
        self.context.opaque_struct_type("IO");
        self.context.opaque_struct_type("String");
        self.context.opaque_struct_type("Int");
        self.context.opaque_struct_type("Bool");
    }

    fn code_native_class_structs(&self) {
        // vtable
        let object_attrs = &[BasicTypeEnum::PointerType(
            self.context.ptr_type(self.aspace),
        )];
        let typ = self.context.get_struct_type("Object").unwrap();
        typ.set_body(object_attrs, false);

        // vtable, value
        let io_attrs = &[BasicTypeEnum::PointerType(
            self.context.ptr_type(self.aspace),
        )];
        let typ = self.context.get_struct_type("IO").unwrap();
        typ.set_body(io_attrs, false);

        // vtable, value
        let int_attrs = &[
            BasicTypeEnum::PointerType(self.context.ptr_type(self.aspace)),
            BasicTypeEnum::IntType(self.context.i32_type()),
        ];
        let typ = self.context.get_struct_type("Int").unwrap();
        typ.set_body(int_attrs, false);

        // vtable, value
        let bool_attrs = &[
            BasicTypeEnum::PointerType(self.context.ptr_type(self.aspace)),
            BasicTypeEnum::IntType(self.context.bool_type()),
        ];
        let typ = self.context.get_struct_type("Bool").unwrap();
        typ.set_body(bool_attrs, false);

        // vtable, str length, str content
        let string_attrs = &[
            BasicTypeEnum::PointerType(self.context.ptr_type(self.aspace)),
            BasicTypeEnum::PointerType(self.context.ptr_type(self.aspace)),
            BasicTypeEnum::ArrayType(self.context.i8_type().array_type(0)),
        ];
        let typ = self.context.get_struct_type("String").unwrap();
        typ.set_body(string_attrs, false);
    }

    fn sym_to_llvm_type(&self, typ: &Sym) -> BasicTypeEnum {
        if self.ct.native_classes.contains(typ) || self.ct.program_classes.contains(typ) {
            return BasicTypeEnum::StructType(
                self.context
                    .get_struct_type(typ)
                    .unwrap_or_else(|| panic!("No struct type for {}", typ)),
            );
        }
        panic!("Unknown type {}.", typ);
    }

    fn code_class_struct(&self, name: &Sym) {
        let mut all_attr_types = vec![BasicTypeEnum::PointerType(
            self.context.ptr_type(self.aspace),
        )];
        let other_attr_types_vec: Vec<BasicTypeEnum> = self
            .ct
            .get_all_attrs(name)
            .iter()
            .map(|(_name, typ, _)| self.sym_to_llvm_type(typ))
            .collect();
        // vtable
        all_attr_types.extend(other_attr_types_vec);
        let typ = self.context.get_struct_type(name).unwrap();
        typ.set_body(&all_attr_types, false);
    }

    pub fn code_all_class_structs(&self) {
        // Must do all declarations before doing any coding.

        // Declarations of native class structs...
        self.declare_native_class_struct_types();
        // ...and non-native class structs types.
        for class in self.ct.program_classes.iter() {
            self.context.opaque_struct_type(class);
        }

        // Coding of native class structs...
        self.code_native_class_structs();
        // ...and non-native class structs types.
        for class in self.ct.program_classes.iter() {
            self.code_class_struct(class)
        }
    }
}
