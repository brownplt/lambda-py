class A:
    def f(self):
        return 'A'
    @classmethod
    def cm(cls):
        return (cls, 'A')

class B(A):
    def f(self):
        return super().f() + 'B'
    @classmethod
    def cm(cls):
        return (cls, super().cm(), 'B')

class C(A):
    def f(self):
        return super().f() + 'C'
    @classmethod
    def cm(cls):
        return (cls, super().cm(), 'C')

class D(C, B):
    def f(self):
        return super().f() + 'D'
    def cm(cls):
        return (cls, super().cm(), 'D')

class E(D):
    pass

class F(E):
    f = E.f

class G(A):
    pass

___assertEqual(D().f(), 'ABCD')

___assertEqual(D.f(D()), 'ABCD')

___assertEqual(E().f(), 'ABCD')
___assertEqual(E.f(E()), 'ABCD')

___assertEqual(F().f(), 'ABCD')
___assertEqual(F.f(F()), 'ABCD')

___assertEqual(A.cm(), (A, 'A'))
___assertEqual(A().cm(), (A, 'A'))
___assertEqual(G.cm(), (G, 'A'))
___assertEqual(G().cm(), (G, 'A'))

d = D()
___assertEqual(d.cm(), (d, (D, (D, (D, 'A'), 'B'), 'C'), 'D'))
e = E()
___assertEqual(e.cm(), (e, (E, (E, (E, 'A'), 'B'), 'C'), 'D'))

# Issue4360: super() did not work in a function that
# contains a closure
class EE(A):
    def f(self):
        def nested():
            self
        return super().f() + 'E'

___assertEqual(EE().f(), 'AE')
