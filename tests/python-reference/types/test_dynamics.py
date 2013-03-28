def test_dynamics():
    # Testing class attribute propagation...
    class D(object):
        pass
    class E(D):
        pass
    class F(D):
        pass
    D.foo = 1
    ___assertEqual(D.foo, 1)
    # Test that dynamic attributes are inherited
    ___assertEqual(E.foo, 1)
    ___assertEqual(F.foo, 1)
    # Test dynamic instances
    class C(object):
        pass
    a = C()
    ___assertFalse(hasattr(a, "foobar"))
    C.foobar = 2
    ___assertEqual(a.foobar, 2)
    C.method = lambda self: 42
    ___assertEqual(a.method(), 42)
    C.__repr__ = lambda self: "C()"
    # ___assertEqual(repr(a), "C()") NB: We don't have repr() yet. (Alejandro)
    ___assertEqual(a.__repr__(), "C()")
    C.__int__ = lambda self: 100
    ___assertEqual(int(a), 100)
    ___assertEqual(a.foobar, 2)
    ___assertFalse(hasattr(a, "spam"))
    def mygetattr(self, name):
        if name == "spam":
            return "spam"
        raise AttributeError
    C.__getattr__ = mygetattr
    ___assertEqual(a.spam, "spam")
    a.new = 12
    ___assertEqual(a.new, 12)
    def mysetattr(self, name, value):
        if name == "spam":
            raise AttributeError
        return object.__setattr__(self, name, value)
    C.__setattr__ = mysetattr
    try:
        a.spam = "not spam"
    except AttributeError:
        pass
    else:
        assert False, "expected AttributeError"
    ___assertEqual(a.spam, "spam")
    class D(C):
        pass
    d = D()
    d.foo = 1
    ___assertEqual(d.foo, 1)
    """ NB: this fails with object has no __int__ attribute (Alejandro)
    # Test handling of int*seq and seq*int
    class I(int):
        pass
    ___assertEqual("a"*I(2), "aa")
    ___assertEqual(I(2)*"a", "aa")
    ___assertEqual(2*I(3), 6)
    ___assertEqual(I(3)*2, 6)
    ___assertEqual(I(3)*I(2), 6)
    """

    # NB: we don't handle setting metaclass yet...
    # Test comparison of classes with dynamic metaclasses
    class dynamicmetaclass(type):
        pass
    class someclass(metaclass=dynamicmetaclass):
        pass
    #___assertNotEqual(someclass, object)
    ___assertFalse(someclass == object)
    

test_dynamics()
