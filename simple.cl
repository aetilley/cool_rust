(* A simple program in the cool language *)

class Apple {
  b: Int <- 42;
};

class Orange inherits Apple {
  c: String <- "hello world\n\n";
  foo() : String {"heya"};
};


class Banana {
    carol: Apple;

    bar() : Orange {new Orange};
};

class Main {
    main() : Object {(new IO).out_string("Hello world!")};


};
