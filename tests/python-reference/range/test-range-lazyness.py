# make a list with the first n elements of iterable
def list_take(iterable, n):
    result = []
    for i in iterable:
        if n > 0:
            result.append(i)
        else:
            break
        n -= 1
    return result

# 2**30, enough to give an error if a list of this size is ever built,
# at least on 32bit machines
N = 1073741824
# small number of elements to be taken from the iterable
n = 5

# test lazyness of range
___assertEqual(list_take(range(N), n), list(range(n)))
