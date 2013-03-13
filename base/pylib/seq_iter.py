class SeqIter:
    def __init__(self,l):
        self.l = l
        self.i = 0
        self.stop = False

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
            self.l.__len__()
        except AttributeError:
            has_length = False

        try:
            if self.stop:
                raise StopIteration()
            if has_length and self.i >= self.l.__len__():
                self.stop = True
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

___assign("%SeqIter", SeqIter)

def iter(l, *args):
    callable = ___id("%callable")
    
    if args.__len__() == 1:
        if callable(l):
            stopwhen = args[0]
            return FuncIter(l, stopwhen)
        else:
            TypeError("iter(v, w): v must be callable")
    elif args.__len__() == 0:
        try:
            return l.__iter__()
        except:
            try:
                if callable(l.__getitem__):
                    return SeqIter(l)
            except:
                raise TypeError("object is not iterable")
    else:
        raise TypeError("iter expect at most 2 arguments")

___assign("%iter", iter)

def next(it):
    return it.__next__()

___assign("%next", next)

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

___assign("%FuncIter", FuncIter)
