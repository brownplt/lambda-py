def make_adder(x):
    def adder(y):
        return x + y
    return adder

inc = make_adder(1)
plus10 = make_adder(10)

___assertEqual(inc(1), 2)
___assertEqual(plus10(-2), 8)
