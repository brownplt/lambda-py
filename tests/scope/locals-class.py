# This test verifies that calling locals() does not pollute
# the local namespace of the class with free variables.  Old
# versions of Python had a bug, where a free variable being
# passed through a class namespace would be inserted into
# locals() by locals() or exec or a trace function.
#
# The real bug lies in frame code that copies variables
# between fast locals and the locals dict, e.g. when executing
# a trace function.

def f(x):
    class C:
        x = 12
        def m(self):
            return x
        locals()
    return C

___assertEqual(f(1).x, 12)

def f(x):
    class C:
        y = x
        def m(self):
            return x
        z = list(locals())
    return C

varnames = f(1).z
___assertNotIn("x", varnames)
___assertIn("y", varnames)
