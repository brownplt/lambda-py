
x = 1
g = globals()
___assertEqual(g['x'], 1)

g['x'] = 2
___assertEqual(x, 2)

