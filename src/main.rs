use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use cool_rust::ast::Program;
use cool_rust::ast_parse::Parse;

fn main() {
    let user_args: Vec<String> = env::args().collect();
    let path = Path::new(user_args.get(1).expect("Pass the path of a file to compile."));
    let display = path.display();

    let mut file = match File::open(path) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };

    let mut code = String::new();
    if let Err(why) = file.read_to_string(&mut code) {
        panic!("couldn't read {}: {}", display, why);
    }

    // Tokenize and Parse
    let mut program: Program = Program::parse(&code).expect("Main program did not parse");

    // Semantic analysis.
    program.semant().expect("Program did not typecheck");

    // Codegen
    let out_file = &format!("{}.ll", path.file_stem().unwrap().to_str().unwrap());
    program.to_llvm(out_file);
}
