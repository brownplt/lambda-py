def f():
    n = yield 1
    yield n

g = f()
assert next(g) == 1
assert next(g,2) == 2
