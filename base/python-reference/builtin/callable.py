___assertTrue(callable(len))
___assertFalse(callable("a"))
___assertTrue(callable(callable))
___assertTrue(callable(lambda x, y: x + y))

def f(): pass
___assertTrue(callable(f))

class C1:
    def meth(self): pass
___assertTrue(callable(C1))
c = C1()
___assertTrue(callable(c.meth))
___assertFalse(callable(c))

