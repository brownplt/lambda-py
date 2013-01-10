# Should produce a syntax error

x = 9

def f():
	nonlocal x
	return x
