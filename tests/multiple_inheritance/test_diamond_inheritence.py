# Testing multiple inheritance special cases...
class A(object):
    def spam(self): return "A"
___assertEqual(A().spam(), "A")
class B(A):
    def boo(self): return "B"
    def spam(self): return "B"
___assertEqual(B().spam(), "B")
___assertEqual(B().boo(), "B")
class C(A):
    def boo(self): return "C"
___assertEqual(C().spam(), "A")
___assertEqual(C().boo(), "C")
class D(B, C): pass
___assertEqual(D().spam(), "B")
___assertEqual(D().boo(), "B")
___assertEqual(D.__mro__, (D, B, C, A, object))
class E(C, B): pass
___assertEqual(E().spam(), "B")
___assertEqual(E().boo(), "C")
___assertEqual(E.__mro__, (E, C, B, A, object))
# MRO order disagreement
try:
    class F(D, E): pass
except TypeError:
    pass
else:
    ___fail() # expected MRO order disagreement (F)
try:
    class G(E, D): pass
except TypeError:
    pass
else:
    ___fail() # expected MRO order disagreement (F)
