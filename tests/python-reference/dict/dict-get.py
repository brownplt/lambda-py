d = {}
___assertIs(d.get('c'), None)
___assertEqual(d.get('c', 3), 3)
d = {'a': 1, 'b': 2}
___assertIs(d.get('c'), None)
___assertEqual(d.get('c', 3), 3)
___assertEqual(d.get('a'), 1)
___assertEqual(d.get('a', 3), 1)
___assertRaises(TypeError, d.get)
#___assertRaises(TypeError, d.get, None, None, None)
