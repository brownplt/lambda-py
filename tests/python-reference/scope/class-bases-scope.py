# Test scoping of bases in class definition
def f():
    # C should be in the local scope
    class C:
        pass
    # if bases lookup is global this should fail
    class S(C):
        pass
    return S

# Now we add S to the global scope and use it
S = f()
s = S()
___assertTrue(isinstance(s, S))

# But C shouldn't be in the global scope
try:
    c = C()
except NameError:
    pass
else:
    ___assertFail()
