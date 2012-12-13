def f(x):
    def g(y):
        def h(z):
            return y + z
        w = x + y
        y += 3
        return locals()
    return g

d = f(2)(4)
___assertIn('h', d)
del d['h']
___assertEqual(d, {'x': 2, 'y': 7, 'w': 6})
