use crate::ast::Program;
use crate::codegen_manager::CodeGenManager;

use inkwell::context::Context;
use inkwell::values::IntValue;

impl Program {
    pub fn to_llvm(&self) -> String {
        let context = Context::create();
        let cgm = CodeGenManager::init(&context);
        let _ = self.codegen(&cgm);
        format!("{}", cgm.module.print_to_string())
    }

    // Would be nice to make this a trait, but not clear what to make the
    // return value.
    fn codegen<'a>(&self, cgm: &CodeGenManager<'a>) -> IntValue<'a> {
        cgm.context.i64_type().const_int(42, false)
    }
}
