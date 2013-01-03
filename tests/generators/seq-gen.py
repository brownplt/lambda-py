def seq():
	x = 10
	yield x
	y = 12
	yield y
	z = 20

g = seq()
___assertEqual(10, next(g))
___assertEqual(12, next(g))
___assertRaise(StopIteration, next, g)