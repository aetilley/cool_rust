A compiler for the Cool language written in Rust with an LLVM backend. 

COOL Language manual:  https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

Example Program (`hello.cl`)

```
class Apple {
    greet() : Object {(new IO).out_string("Hello World!")};
};

class Orange inherits Apple {
    greet() : Object {(new IO).out_string("Hola Mundo!")};
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
// Compile to LLVM Intermediate Representation. (output is hello.ll)
$ cargo run hello.cl
// Compile IR file with Clang toolchain.
$ llc hello.ll && clang hello.s -c && clang hello.o -o hello && ./hello
Hello World!
Hola Mundo!
```

A more interesting example involves Cool's `case` expression:

```
class A{};
class A1 inherits A{};
class A2 inherits A{};
class A11 inherits A1{};
class A12 inherits A1{};
class A21 inherits A2{};
class A22 inherits A2{};
class A111 inherits A11{};
class A112 inherits A11{};
class A121 inherits A12{};
class A122 inherits A12{};

class Main {
    io: IO <- new IO; 

    x : A <- new A112;
    y : A <- new A121;

    foo(a: A) : Object {
        case a of  
        a : A1 => io.out_string("A1");
        a : A2 => io.out_string("A2");
        a : A11 => io.out_string("A11");
        a : A21 => io.out_string("A21");
        a : A22 => io.out_string("A22");
        a : A111 => io.out_string("A111");
        a : A122 => io.out_string("A122");
        esac
    };  

    main() : Object {{
      foo(x);
      foo(y);
    }}; 
};
```

From the manual:
> Case expressions provide runtime type tests on objects. First, `expr0` is evaluated and its dynamic type C noted (if expr0 evaluates to void a run-time error is produced). Next, from among the branches the branch with the least type `typek` such that `C` ≤ `typek` is selected. The identifier `idk` is bound to the value of `expr0` and the expression `exprk` is evaluated. The result of the case is the value of `exprk`. 

```
$ cargo run case.cl && llc case.ll && clang case.s -c && clang case.o -o case && ./case
A11
A1
```

Note this package requires LLVM (version <= 18 for now, but keep an eye on https://github.com/TheDan64/inkwell/pull/557). Furthermore, running the full test suite requires that you have an LLVM_PATH environment variable set, and that it points to a directory that contains a `bin/llvm[@version]` and a `bin/clang`.
