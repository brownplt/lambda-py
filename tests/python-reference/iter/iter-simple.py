# Helper to check that an iterator returns a given sequence
def check_iterator(it, seq):
    res = []
    while 1:
        try:
            val = next(it)
        except StopIteration:
            break
        res.append(val)

    ___assertEqual(res, seq)

# Helper to check that a for loop generates a given sequence
def check_for_loop(expr, seq):
    res = []
    for val in expr:
        res.append(val)
    ___assertEqual(res, seq)

# Test basic use of iter() function
check_iterator(iter(range(10)), list(range(10)))

# Test that iter(iter(x)) is the same as iter(x)
seq = list(range(10))
it = iter(seq)
it2 = iter(it)
___assertTrue(it is it2)

check_for_loop(iter(range(10)), list(range(10)))

check_for_loop(iter([]), [])
