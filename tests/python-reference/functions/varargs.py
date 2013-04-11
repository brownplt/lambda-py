
def f(*a):
    return 0
___assertEqual(f(), 0)
___assertEqual(f(1, 2, 3), 0)

f = lambda *a: 0
___assertEqual(f(), 0)
___assertEqual(f(4, 5, 6), 0)
