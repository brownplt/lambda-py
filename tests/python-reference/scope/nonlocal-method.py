def f(x):
    class c:
        def inc(self):
            nonlocal x
            x += 1
            return x
        def dec(self):
            nonlocal x
            x -= 1
            return x
    return c()
c = f(0)
___assertEqual(c.inc(), 1)
___assertEqual(c.inc(), 2)
___assertEqual(c.dec(), 1)
___assertEqual(c.dec(), 0)
