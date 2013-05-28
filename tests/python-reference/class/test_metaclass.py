# test_metaclass fragment from standard Python test suite
def test_metaclass():
    # Testing metaclasses...
    class C(metaclass=type):
        def __init__(self):
            self.__state = 0
        def getstate(self):
            return self.__state
        def setstate(self, state):
            self.__state = state
    a = C()
    ___assertEqual(a.getstate(), 0)
    a.setstate(10)
    ___assertEqual(a.getstate(), 10)
    class _metaclass(type):
        def myself(cls): return cls
    class D(metaclass=_metaclass):
        pass
    ___assertEqual(D.myself(), D)
    d = D()
    ___assertEqual(d.__class__, D)
    class M1(type):
        def __new__(cls, name, bases, dict):
            dict['__spam__'] = 1
            return type.__new__(cls, name, bases, dict)
    class C(metaclass=M1):
        pass
    ___assertEqual(C.__spam__, 1)
    c = C()
    ___assertEqual(c.__spam__, 1)
    
test_metaclass()
