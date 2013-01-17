# Testing error messages for MRO disagreement...
class A(object): pass
class B(A): pass
class C(object): pass

# Test some very simple errors
# Duplicate base class A
#___assertRaises(TypeError, type, "X", (A, A), {}) # requires type(name, bases, dict)
try:
    class X(A, A): pass
except TypeError:
    pass
else:
    ___fail() # expected "duplicate base class A"
# Cannot create a consistent method resolution order (MRO) for bases
#___assertRaises(TypeError, type, "X", (A, B), {}) # requires type(name, bases, dict)
try:
    class X(A, B): pass
except TypeError:
    pass
else:
    ___fail() # expected "Cannot create a consisten method resolution order for bases B, A"
# Cannot create a consistent method resolution order (MRO) for bases
#___assertRaises(TypeError, type, "X", (A, C, B), {}) # requires type(name, bases, dict)
try:
    class X(A, C, B): pass
except TypeError:
    pass
else:
    ___fail() # expected "Cannot create a consisten method resolution order for bases B, A, C"
# Test a slightly more complex error
class GridLayout(object): pass
class HorizontalGrid(GridLayout): pass
class VerticalGrid(GridLayout): pass
class HVGrid(HorizontalGrid, VerticalGrid): pass
class VHGrid(VerticalGrid, HorizontalGrid): pass
# Cannot create a consistent method resolution order (MRO) for bases
#___assertRaises(TypeError, type, "ConfusedGrid", (HVGrid, VHGrid), {}) # requires type(name, bases, dict)
try:
    class ConfusedGrid(HVGrid, VHGrid): pass
except TypeError:
    pass
else:
    ___fail() # expected "Cannot create a consisten method resolution order for bases VerticalGrid, HorizontalGrid"
