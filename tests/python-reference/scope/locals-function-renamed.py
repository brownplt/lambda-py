# locals called from another identifier
def f(x):
    mylocals = locals
    def g(y):
        def h(z):
            return y + z
        w = x + y
        y += 3
        return mylocals()
    return g

d = f(2)(4)
___assertIn('h', d)
del d['h']
del d['mylocals']
___assertEqual(d, {'x': 2, 'y': 7, 'w': 6})
