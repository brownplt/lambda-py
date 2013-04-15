def f(a, b=2, c=3):
    return (a, b, c)

# mix keywords, **expression and defaults
___assertEqual(f(1, **{'b':22, 'c':33}), (1, 22, 33))
___assertEqual(f(a=1, **{'b':22, 'c':33}), (1, 22, 33))
___assertEqual(f(1, **{'b':22}), (1, 22, 3))
___assertEqual(f(**{'a':1, 'b':22}), (1, 22, 3))
___assertEqual(f(1, **{'c':33}), (1, 2, 33))
___assertEqual(f(**{'a':1, 'c':33}), (1, 2, 33))
