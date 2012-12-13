def f(x):
    def g(y):
        x = 42 # check that this masks binding in f()
        def h(z):
            return x + z
        return h
    return g(2)

test_func = f(10)
___assertEqual(test_func(5), 47)
