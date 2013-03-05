class A: pass
a = A()
f = lambda x: x
f(a).x = 1 # this assignment does not work in current implementation
___assertEqual(a.x, 1)
