
class tuple(object):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      return ()
    else:
      first_arg = ___delta("tuple-getitem", args, 0)
      return first_arg.__tuple__()

  def __init__(self, *args):
    pass

  def __len__(self):
    int = ___id("%int")
    return ___delta("tuple-len", self, int)

  def __getitem__(self, f):
    return ___delta("tuple-getitem", self, f)

  def __tuple__(self): return self

  def __add__(self, other):
    tuple = ___id("%tuple")
    return ___delta("tuple+", self, other, tuple)

  def __mult__(self, other):
    tuple = ___id("%tuple")
    return ___delta("tuple*", self, other, tuple)

  def __in__(self, other):
    c = 0
    while c < self.__len__():
      if self.__getitem__(c).__eq__(other):
        return True
      c = c.__add__(1)
    return False

  def __str__(self):
    str = ___id("%str")
    return ___delta("tuple-str", self, str)

  def __bool__(self):
    return not ___delta("num=", self.__len__(), 0)

  def __iter__(self):
    SeqIter = ___id("%SeqIter")
    return SeqIter(self)

  # NOTE(joe): copied code (list.py)
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
    return lstcmp(self, other, 0)

  def __eq__(self, other):
    cmpresult = self.__cmp__(other)
    return cmpresult.__eq__(0)

  def __hash__(self):
    for elt in self:
      result += self.__hash__() * 17
    return result

  def __list__(self):
    return SeqIter(self).__list__()

  def __set__(self):
    set = ___id("%set")
    return ___delta("tuple-set", self, set)

___assign("%tuple", tuple)
