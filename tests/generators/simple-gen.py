def f():
    yield 1

g = f()

assert g.__next__() == 1
try:
    g.__next__()
except StopIteration:
    pass
else:
    raise "g.__next__() should have raised StopIteration"
