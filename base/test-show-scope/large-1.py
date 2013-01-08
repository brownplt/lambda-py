# Shouldn't create new let in function body, since
# x has already been rebound

x = 5

def f(x, y, z):
	x = y + z
	return x
