class C:
    pass
class D(C):
    pass
class E:
    pass
class F(C, E):
    pass
c = C()
d = D()
e = E()
f = F()
___assertTrue(isinstance(c, (C, E)))
___assertTrue(isinstance(d, (E, C)))
___assertTrue(not isinstance(e, (C, F)))
___assertTrue(not isinstance(c, (F, D)))
___assertTrue(not isinstance('foo', (C, D, E, F)))
___assertTrue(isinstance(f, (E,)))
___assertTrue(isinstance(f, (C,)))