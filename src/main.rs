use std::env;
use std::path::Path;
use std::fs::File;
use std::io::Read;

mod token;
use token::{tokenize_all, Token};

fn main() {

	let user_args: Vec<String> = env::args().collect();
    let path = Path::new(&user_args[1]);
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", display, why),
        Ok(file) => file,
    };

    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => panic!("couldn't read {}: {}", display, why),
        Ok(_) => (), 
    }

    let result: Vec::<Token> = tokenize_all(&code);
    for token in result {
        print!("{}\n", token);
    }
    
}
