def f(x):
    def inc():
        nonlocal x
        x += 1
        return x
    def dec():
        nonlocal x
        x -= 1
        return x
    return inc, dec

inc, dec = f(0)
___assertEqual(inc(), 1)
___assertEqual(inc(), 2)
___assertEqual(dec(), 1)
___assertEqual(dec(), 0)
