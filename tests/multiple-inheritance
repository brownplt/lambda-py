# test the use of decorators to memoize functions
def memo(func):
    cache = {}
    def wrap(*args):
        if args not in cache:
            cache[args] = func(*args)
        return cache[args]
    return wrap

# memoized fib
@memo
def fib(n):
    if n < 2:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

# this should work even without memoization
___assertEqual(fib(12), 144)
# probably 30 is enough to timeout without memoization
___assertEqual(fib(30), 832040)
