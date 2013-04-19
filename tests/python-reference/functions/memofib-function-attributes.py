# test use of function atributes, in this case to memoize fib.
def memofib(n):
    try:
        if n in memofib.cache: return memofib.cache[n]
    except AttributeError:
        memofib.cache = {}
    memofib.cache[n] = n if n < 2 else memofib(n-1) + memofib(n-2)
    return memofib.cache[n]

___assertEqual(memofib(12), 144)
