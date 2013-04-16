# **kwarg only
___assertEqual((lambda **kwarg: kwarg)(a=1, b=2), {'a':1, 'b':2})

# positional arguments and **kwargs
def f(a, b=2, **kwargs):
    return (a, b, kwargs)

t1 = f(11, 22)
___assertEqual(t1[0], 11)
___assertEqual(t1[1], 22)
___assertEqual(t1[2], {})

t2 = f(11, c=33, **{'d':44})
___assertEqual(t2[0], 11)
___assertEqual(t2[1], 2)
___assertEqual(t2[2], {'c':33, 'd':44})

# positional arg, *args and **kwarg
def g(a, *args, **kwarg):
    return (a, args, kwarg)

t3 = g(11, 2, 3, b=22, c=33)
___assertEqual(t3[0], 11)
___assertEqual(t3[1], (2, 3))
___assertEqual(t3[2], {'b':22, 'c':33})

t4 = g(11, b=22, c=33)
___assertEqual(t4[0], 11)
___assertEqual(t4[1], ())
___assertEqual(t4[2], {'b':22, 'c':33})

t5 = g(11, 2, 3)
___assertEqual(t5[0], 11)
___assertEqual(t5[1], (2, 3))
___assertEqual(t5[2], {})

t6 = g(11)
___assertEqual(t6[0], 11)
___assertEqual(t6[1], ())
___assertEqual(t6[2], {})
