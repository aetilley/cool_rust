use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

pub struct CodeGenManager<'ctx> {
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub context: &'ctx Context,
}

impl<'ctx> CodeGenManager<'ctx> {
    pub fn init(context: &'ctx Context) -> Self {
        let module = context.create_module("cool_module");
        let builder = context.create_builder();

        CodeGenManager {
            builder,
            module,
            context,
        }
    }
}
