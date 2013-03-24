def f():
    try:
        raise StopIteration
    except StopIteration:
        yield 1

    try:
        return
    except StopIteration:
        yield 2

    yield 3

g = f()

assert next(g) == 1

try:
    next(g)
except StopIteration:
    pass
else:
    raise Exception("return should not be catchable within generator")
