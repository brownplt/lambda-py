# test the issue https://github.com/brownplt/lambda-py/commit/e03dd81cdfb17bccd16bb385f5c1df74fa9c58c3
def foo():
    for i in [1,2,3,4]:
        pass
def bar():
    x, y = 1, 2
    
foo()
bar()

try:
    i += 1
    ___fail("i shouldn't be in global scope")
except NameError:
    pass
else:
    ___fail("unexpected failure")

try:
    if x or y:
        ___fail("x or y shouldn't be in global scope")
except NameError:
    pass
else:
    ___fail("unexpected failure")
