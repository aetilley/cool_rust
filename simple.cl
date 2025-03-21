class Apple {
		greet() : Object {(new IO).out_string("I'm an Apple!")};
};

class Orange inherits Apple {
		greet() : Object {(new IO).out_string("I'm an Orange")};
};

class Main {
    main() : 
				Object {
				{
					(new Apple).greet();
					(new Orange).greet();
				}
		};
};

