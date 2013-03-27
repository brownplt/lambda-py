def basic():
	yield 1
	yield 2

g = basic()
___assertEqual(1, next(g))
___assertEqual(2, next(g))
___assertRaises(StopIteration, next, g)

def f():
	x = 0
	while x < 5:
		yield x
		x += 1

g = f()
# generator object should return itself when calling iter
___assertIs(g, iter(g))
___assertEqual(list(g), list(range(5)))
___assertEqual(list(g), [])
