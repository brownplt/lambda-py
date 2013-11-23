def f():
    x = 0
    def inc():
        nonlocal x
        x += 1
        return x
    def dec():
        x = 1
        x -= 1
        return x
    return inc, dec

inc, dec = f()
___assertEqual(inc(), 1)
___assertEqual(inc(), 2)
___assertEqual(dec(), 0)
___assertEqual(dec(), 0)
___assertEqual(inc(), 3)
