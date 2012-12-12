d = {}
___assertEqual(set(d.values()), set())
d = {1:2}
___assertEqual(set(d.values()), {2})
___assertRaises(TypeError, d.values, None)
