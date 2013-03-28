x = 12
y = 13

def f():
 x, y = (3,4)
f()

assert x == 12
assert y == 13

class c:
 x, y = (5, 6)

assert x == 12
assert y == 13

assert c().x == 5
assert c().y == 6

