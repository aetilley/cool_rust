WIP!

COOL Language manual:  https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

Status:  Parsing complete.  Semantic analysis almost complete.

Next up:  LLVM backend!

Usage:  

`$ cargo test`

`$ cargo run -- simple.cl`

Warts:  Currently we're just hard-coding string constants, and obviously we should use a symbol table for interning / comparison (or at the very least pull out many constants in to a dedicated file).  
Will likely use https://docs.rs/symbol_table/latest/symbol_table/  or https://docs.rs/ustr/latest/ustr/index.html.  Will decide on this soon.
