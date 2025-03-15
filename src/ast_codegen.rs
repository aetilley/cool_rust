use crate::ast::Program;
use crate::class_table::ClassTable;
use crate::codegen_manager::CodeGenManager;

use inkwell::context::Context;

impl Program {
    pub fn to_llvm(&self) {
        let context = Context::create();
        let ct = ClassTable::new(&self.classes).expect(
            "Failure to construct ClassTable \
            from program (should have been caught \
            in semantic analysis).",
        );
        let cgm = CodeGenManager::init(&context, ct);
        //
        cgm.code_all_class_structs();
        //
        cgm.code_all_inits();
        //
        cgm.code_all_methods();
        //
        cgm.module.print_to_file("out.ll").unwrap();
    }
}
