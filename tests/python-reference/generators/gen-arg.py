def f():
    n = yield 1
    yield n

g = f()
assert next(g) == 1
assert g.send(2) == 2
