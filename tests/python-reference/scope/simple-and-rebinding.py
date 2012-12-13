def make_adder3(x):
    def adder(y):
        return x + y
    x = x + 1 # check tracking of assignment to x in defining scope
    return adder

inc = make_adder3(0)
plus10 = make_adder3(9)

___assertEqual(inc(1), 2)
___assertEqual(plus10(-2), 8)
