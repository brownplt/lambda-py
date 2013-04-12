d = {'a': 1, 'm':lambda self: self}

C = type("C", (object,), d)

assert C.a == 1
assert hasattr(C, 'm')

c=C()
assert c.m() == c
