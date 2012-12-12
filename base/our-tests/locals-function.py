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
print(d)
del d['h']
print(d=={'x': 2, 'y': 7, 'w': 6})
___assertEqual(d, {'x': 2, 'y': 7, 'w': 6})
