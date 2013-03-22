def grange(n):
	for i in range(n):
		yield i

def outergen(n):
	for i in grange(n):
		yield i

___assertEqual(list(outergen(10)), list(range(10)))