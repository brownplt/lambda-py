# Helper classes

class BasicIterClass:
    def __init__(self, n):
        self.n = n
        self.i = 0
    def __next__(self):
        res = self.i
        if res >= self.n:
            raise StopIteration
        self.i = res + 1
        return res

class IteratingSequenceClass:
    def __init__(self, n):
        self.n = n
    def __iter__(self):
        return BasicIterClass(self.n)

class SequenceClass:
    def __init__(self, n):
        self.n = n
    def __getitem__(self, i):
        if 0 <= i < self.n:
            return i
        else:
            raise IndexError

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


check_for_loop(IteratingSequenceClass(10), list(range(10)))

check_iterator(iter(IteratingSequenceClass(10)), list(range(10)))

check_for_loop(SequenceClass(10), list(range(10)))

check_iterator(iter(SequenceClass(10)), list(range(10)))
