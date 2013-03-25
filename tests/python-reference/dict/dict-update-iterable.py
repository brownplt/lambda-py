d = {}
d.update([[1, 100]])
d.update(((2, 20),))
d.update([(1, 1), (2, 2), (3, 3)])
___assertEqual(d, {1:1, 2:2, 3:3})
