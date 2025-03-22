class Apple {
    greet() : Object {(new IO).out_string("Hello World!")};
};

class Orange inherits Apple {
    greet() : Object {(new IO).out_string("Hola, Mundo!")};
};

class Main {
    a: Apple <- new Apple;
    b: Apple <- new Orange;
    main() : 
    Object {{
        a.greet();
        b.greet();
    }};
};

