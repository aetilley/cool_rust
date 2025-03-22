WIP
Status:  Parsing and Semantic Analysis complete.  Codegen about half complete.

A compiler for the Cool language written in Rust with an LLVM backend. 

COOL Language manual:  https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

Example Program (`simple.cl`)

```
class Apple {
    greet() : Object {(new IO).out_string("Hello World!")};
};

class Orange inherits Apple {
    greet() : Object {(new IO).out_string("Hola, Mundo!")};
};

class Main {
    a: Apple <- new Apple;
    b: Apple <- new Orange;
    main() : Object {
        {
            a.greet();
            b.greet();
        }
    }; 
};
```

Usage:
```
// Compile to LLVM Intermediate Representation. (output is simple.ll)
$ cargo run simple.cl
// Compile IR file with Clang toolchain.
$ llc simple.ll && clang simple.s -c && clang simple.o -o simple && ./simple
Hello World!
Hola, Mundo!
```
