class C:
    pass
class D(C):
    pass
class E:
    pass
c = C()
d = D()
e = E()
___assertTrue(isinstance(c, C))
___assertTrue(isinstance(d, C))
___assertTrue(not isinstance(e, C))
___assertTrue(not isinstance(c, D))
___assertTrue(not isinstance('foo', E))
___assertRaises(TypeError, isinstance, E, 'foo')
___assertRaises(TypeError, isinstance)
