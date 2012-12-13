def make_adder5(x):
    class Adder:
        def __call__(self, y):
            return x + y
    return Adder()

inc = make_adder5(1)
plus10 = make_adder5(10)

___assertEqual(inc(1), 2)
___assertEqual(plus10(-2), 8)
