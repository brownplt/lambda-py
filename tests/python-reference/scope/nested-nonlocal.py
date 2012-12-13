def f(x):
    def g():
        nonlocal x
        x -= 2
        def h():
            nonlocal x
            x += 4
            return x
        return h
    return g

g = f(1)
h = g()
___assertEqual(h(), 3)
