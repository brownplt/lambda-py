# This should produce a Syntax Error

x = 8
class C:
	x = 7
	def f():
		nonlocal x
		return x

