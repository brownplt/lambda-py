
class tuple(object):
  def __init__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      self = ()
    else:
      first_arg = args.__getitem__(0)
      self = first_arg.__tuple__()

  def __len__(self):
    return ___delta("tuple-len", self)

  def __getitem__(self, f):
    return ___delta("tuple-getitem", self, f)

  def __tuple__(self): return self

  def __add__(self, other):
    return ___delta("tuple+", self, other)

  def __mult__(self, other):
    return ___delta("tuple*", self, other)

  def __in__(self, other):
    c = 0
    while c < self.__len__():
      if self.__getitem__(c).__eq__(other):
        return True
      c.__add__(1)
    return False

  def __iter__(self):
    return SeqIter(self)

  def __cmp__(self, other):
    def lstcmp(self, other, idx):
      li1 = self.__getitem__(idx)
      li2 = other.__getitem__(idx)
      if ___prim2("Is", li1, None):
        if ___prim2("Is", li2, None):
          return 0
        else:
          return 1
      else:
        if ___prim2("Is", li2, None):
          return 1
        else:
          cmpval = li1.__cmp__(li2)
          if cmpval.__eq__(0):
            nidx = idx.__add__(1)
            return lstcmp(self, other, nidx)
          else:
            return cmpval
    lstcmp(self, other, 0)

  def __eq__(self, other):
    cmpresult = self.__cmp__(other)
    return cmpresult.__eq__(0)

  def __list__(self):
    return SeqIter(self).__list__()

