d = {'a': 1, 'b': 2}
___assertEqual(d['a'], 1)
___assertEqual(d['b'], 2)
d['c'] = 3
d['a'] = 4
___assertEqual(d['c'], 3)
___assertEqual(d['a'], 4)
del d['b']
___assertEqual(d, {'a': 4, 'c': 3})

___assertRaises(TypeError, d.__getitem__)

