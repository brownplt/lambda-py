def f(a, b, c):
    return (a, b, c)

___assertEqual(f(*[0, 1, 2]), (0, 1, 2))
___assertEqual(f(*"abc"), ('a', 'b', 'c'))
___assertEqual(f(*range(3)), (0, 1, 2))
