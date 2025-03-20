A compiler for the Cool language written in Rust with an LLVM backend. 

COOL Language manual:  https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf


```
$ cargo run simple.cl
$ llc simple.ll && clang simple.s -c && clang simple.o -o simple && ./simple
Hello world!
```

If you find something that is broken or not to spec, please create an issue.
