def f():
	return 1/0

def g():
	yield f()
	yield 42

k = g()
___assertRaises(ZeroDivisionError, next, k)

def f():
	try:
		yield 1
		try:
			yield 2
			1/0
			yield 3 # never get here
		except ZeroDivisionError:
			yield 4
			yield 5
			raise
		except:
			yield 6
		yield 7 # the 'raise' above stops this
	except:
		yield 8
	yield 9
	try:
		x = 12
	finally:
		yield 10
	yield 11

___assertEqual(list(f()), [1, 2, 4, 5, 8, 9, 10, 11])

def h():
	yield 1
	try:
		raise StopIteration
	except:
		yield 2
	yield 3

___assertEqual(list(h()), [1, 2, 3])
