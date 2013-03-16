# Testing error messages for MRO disagreement using type(name, bases, dict)...
class A(object): pass
class B(A): pass
class C(object): pass

# Test some very simple errors
# Duplicate base class A
___assertRaises(TypeError, type, "X", (A, A), {})
# Cannot create a consistent method resolution order (MRO) for bases
___assertRaises(TypeError, type, "X", (A, B), {})
# Cannot create a consistent method resolution order (MRO) for bases
___assertRaises(TypeError, type, "X", (A, C, B), {})

# Test a slightly more complex error
class GridLayout(object): pass
class HorizontalGrid(GridLayout): pass
class VerticalGrid(GridLayout): pass
class HVGrid(HorizontalGrid, VerticalGrid): pass
class VHGrid(VerticalGrid, HorizontalGrid): pass
# Cannot create a consistent method resolution order (MRO) for bases
___assertRaises(TypeError, type, "ConfusedGrid", (HVGrid, VHGrid), {})
