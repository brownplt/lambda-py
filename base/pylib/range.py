# range datatype
class range:
    def __init__(self, *args):
        isinstance = ___id("%isinstance")
        any = ___id("%any")
        if any((not isinstance(arg, int) for arg in args)):
            raise TypeError("range arguments must be int")
        self.start = 0
        self.step = 1
        if args.__len__() == 1:
            self.stop = args[0]
        elif args.__len__() == 2:
            self.start = args[0]
            self.stop = args[1]
        elif args.__len__() == 3:
            self.start = args[0]
            self.stop = args[1]
            self.step = args[2]
            if self.step == 0:
                raise ValueError("range step cannot be 0")
        else:
            raise TypeError("range expect 1 to 3 arguments")
    def __iter__(self):
        range_iterator = ___id("%range_iterator")
        return range_iterator(self)
    def __list__(self):
        result = []
        for e in self:
            result.append(e)
        return result

# range iterator: just iterates over ranges
class range_iterator:
    def __init__(self, rng):
        self.next = rng.start - rng.step
        self.rng = rng
    def __next__(self):
        self.next += self.rng.step
        if ((self.rng.step > 0 and self.next < self.rng.stop) or
            (self.rng.step < 0 and self.next > self.rng.stop)):
            return self.next
        else:
            raise StopIteration()
    def __iter__(self):
        return self
    def __list__(self):
        result = []
        for e in self:
            result.append(e)
        return result

___assign("%range", range)
___assign("%range_iterator", range_iterator)
