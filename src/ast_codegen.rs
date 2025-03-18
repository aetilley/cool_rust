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
        let mut cgm = CodeGenManager::init(&context, ct);
        //
        // cgm.module.set_source_file_name()
        //
        cgm.code_all_class_structs();
        //
        // cgm.code_vtables();
        //
        cgm.register_globals();
        //
        cgm.code_all_inits();
        //
        cgm.code_all_methods();
        //
        cgm.module.verify().unwrap();
        cgm.module.print_to_file("out.ll").unwrap();
    }
}
