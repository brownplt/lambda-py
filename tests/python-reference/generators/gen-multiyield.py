def genfunc1():
	rst = []
	rst.append(0)
	yield rst
	rst = 10
	yield rst
	for x in range(3):
		rst += 1
		yield rst

g = genfunc1()
___assertEqual([0], next(g))
___assertEqual(10, next(g))
___assertEqual(11, next(g))
___assertEqual(12, next(g))
___assertEqual(13, next(g))
___assertRaises(StopIteration, next, g)

def badfunc():
	print("badfunction")

def genfunc2():
	x = 0
	a = yield x
	___assertEqual(a, None)
	x += 2
	yield x
	x = badfunc(x)
	yield x
	x = 'end'

g = genfunc2()
___assertEqual(0, next(g))
___assertEqual(2, next(g))
___assertRaises(TypeError, next, g)
