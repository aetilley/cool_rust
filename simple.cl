class Apple {
  b: Int;
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
    main() : Object {(new IO)@IO.out_string("Buenos Dias, Mundo.")};


};
