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


check_for_loop(iter((0,1,2,3,4,5,6,7,8,9)), list(range(10)))
check_for_loop(iter(range(10)), list(range(10)))
check_for_loop(iter("abcde"), ["a", "b", "c", "d", "e"])

dict = {}
for i in range(10):
    dict[i] = None

check_for_loop(dict, list(dict.keys()))
