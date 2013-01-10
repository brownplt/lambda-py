def f():
	x = 8
	def g():
		nonlocal x
		x = 9
		return x
