d = {}
___assertNotIn('a', d)
___assertFalse('a' in d)
___assertTrue('a' not in d)
d = {'a': 1, 'b': 2}
___assertIn('a', d)
___assertIn('b', d)
___assertNotIn('c', d)
