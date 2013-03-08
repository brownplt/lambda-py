class SeqIter:
    def __init__(self,l):
        self.l = l
        self.i = 0

    def __len__(self):
        return len(self.l)

    def __list__(self):
        l = []
        while True:
            try:
                l.append(self.__next__())
            except StopIteration:
                break
        return l



    def __iter__(self):
        return self

    def __next__(self):
        has_length = True
        found = False
        try:
            len(self.l)
        except AttributeError:
            has_length = False

        try:
            if has_length and self.i >= len(self.l):
                raise StopIteration()
            ret = self.l[self.i]
            found = True
        except IndexError:
            raise StopIteration()
        except StopIteration:
            raise StopIteration()

        self.i += 1
        if found:
          return ret
        else:
          return None

def iter(l, *args):
    if len(args) == 1:
        stopwhen = args[0]
        return FuncIter(l, stopwhen)
    else:
        try:
            return l.__iter__()
        except:
            raise TypeError()

___assign("%iter", iter)

def next(it):
    return it.__next__()

class FuncIter:
    def __init__(self, func, stopwhen):
        self.func = func
        self.stopwhen = stopwhen
        self.stopped = False

    def __list__(self):
        l = []
        while not self.stopped:
            try:
                l.append(self.__next__())
            except StopIteration:
                break
        return l

    def __next__(self):
        f = self.func
        v = f()

        if v == self.stopwhen:
            self.stopped = True
            raise StopIteration()
        else:
            return v
