use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use cool_rust::ast::Program;
use cool_rust::ast_parse::Parse;
//use cool_rust::ast_codegen::compile;

fn main() {
    let user_args: Vec<String> = env::args().collect();
    let path = Path::new(&user_args[1]);
    let display = path.display();

    let mut file = match File::open(path) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };

    let mut code = String::new();
    if let Err(why) = file.read_to_string(&mut code) {
        panic!("couldn't read {}: {}", display, why);
    }

    //Tokenize (TODO:  Create a flag to trigger this only.)
    // use cool_rust::token::{Token, tokenize_all};
    // let result: Vec::<Token> = tokenize_all(&code);
    // for token in result {
    //     print!("{}\n", token);
    // }

    // Parse
    let mut program: Program = Program::parse(&code).expect("Main program did not parse");

    program.semant().expect("Program did not typecheck");

    // println!("{:#?}", program)

    // print to out.ll
    program.to_llvm();
}
