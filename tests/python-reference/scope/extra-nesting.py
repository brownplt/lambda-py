def make_adder2(x):
    def extra(): # check freevars passing through non-use scopes
        def adder(y):
            return x + y
        return adder
    return extra()

inc = make_adder2(1)
plus10 = make_adder2(10)

___assertEqual(inc(1), 2)
___assertEqual(plus10(-2), 8)
