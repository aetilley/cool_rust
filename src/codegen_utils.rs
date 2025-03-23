use crate::symbol::Sym;
use inkwell::values::{ArrayValue, IntValue};

use crate::codegen::CodeGenManager;

impl<'ctx> CodeGenManager<'ctx> {
    pub fn code_array_value_from_sym(&self, s: &Sym) -> ArrayValue<'ctx> {
        let array_values: Vec<IntValue> = s
            .as_bytes()
            .iter()
            .map(|byt| self.context.i8_type().const_int(*byt as u64, false))
            .collect();
        self.context.i8_type().const_array(&array_values[..])
    }
}
