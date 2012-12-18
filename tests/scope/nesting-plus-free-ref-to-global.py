def make_adder6(x):
    global global_nest_x
    def adder(y):
        return global_nest_x + y
    global_nest_x = x
    return adder

inc = make_adder6(1)
plus10 = make_adder6(10)

___assertEqual(inc(1), 11) # there's only one global
___assertEqual(plus10(-2), 8)
