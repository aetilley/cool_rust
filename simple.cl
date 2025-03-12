class Apple {
  b: Int <- 42;
};

class Orange inherits Apple {
  foo() : Str {new Str};
};


class Banana {
    carol: Apple;

    bar(x: Orange) : Apple {if true then carol else x fi};
};
