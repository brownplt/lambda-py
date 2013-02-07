# see thread python-dev/2002-October/029035.html
# Testing ex5 from C3 switch discussion...
class A(object): pass
class B(object): pass
class C(object): pass
class X(A): pass
class Y(A): pass
class Z(X,B,Y,C): pass
___assertEqual(Z.__mro__, (Z, X, B, Y, A, C, object))
