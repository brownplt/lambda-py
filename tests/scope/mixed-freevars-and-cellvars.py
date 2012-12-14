def identity(x):
    return x

def f(x, y, z):
    def g(a, b, c):
        a = a + x # 3
        def h():
            # z * (4 + 9)
            # 3 * 13
            return identity(z * (b + y))
        y = c + z # 9
        return h
    return g

g = f(1, 2, 3)
h = g(2, 4, 6)
___assertEqual(h(), 39)
