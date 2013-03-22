def f():
    n = 1
    while n < 10:
        yield n
        n += 1

assert [x for x in f()] == [1,2,3,4,5,6,7,8,9]
