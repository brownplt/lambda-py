def fun():
	for x in range(6):
		for y in [-1, 4, 2, 1, 7]:
			if (x > y):
				yield x*2+y

gencomp = (x*2+y for x in range(6) for y in [-1, 4, 2, 1, 7] if x > y)

___assertEqual(list(fun()), list(gencomp))

def filgen(f, it):
	if (f == None):
		for x in it:
			if (x):
				yield x
	else:
		for x in it:
			if (f(x)):
				yield x

f = lambda c: 'a' <= c <= 'z'
it = 'Hello World'
___assertEqual(list(filter(f, it)), list(filgen(f, it)))
___assertEqual([1, 'hello', [3], 9], list(filgen(None, [1, 'hello', [], [3], '', None, 9, 0])))
___assertEqual([1, 9, 2], list(filgen(lambda x: x > 0, [1, -3, 9, 0, 2])))
