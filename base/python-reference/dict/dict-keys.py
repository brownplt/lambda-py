d = {}
___assertEqual(set(d.keys()), set())
d = {'a': 1, 'b': 2}
k = d.keys()
___assertIn('a', d)
___assertIn('b', d)
___assertRaises(TypeError, d.keys, None)
