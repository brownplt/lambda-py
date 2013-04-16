# keyword-only arguments
def f(*args, a, b=2):
    return (args, a, b)

___assertEqual(f(1, 2, a=11, b=22), ((1, 2), 11, 22))
___assertEqual(f(1, 2, a=11), ((1, 2), 11, 2))

# keyword-only arguments and **kwarg
def g(*args, a=1, b=2, **kwarg):
    return (args, a, b, kwarg)

t1 = g(1, 2, a=11, b=22, c=33)
___assertEqual(t1[0], (1, 2))
___assertEqual(t1[1], 11)
___assertEqual(t1[2], 22)
___assertEqual(t1[3], {'c':33})
t2 = g(1, 2, 3)
___assertEqual(t2[0], (1, 2, 3))
___assertEqual(t2[1], 1)
___assertEqual(t2[2], 2)
___assertEqual(t2[3], {})
