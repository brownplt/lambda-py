
class list(object):
  def __init__(self, other):
    if (type(other) == list):
      self = ___delta("list-copy", other, list)
    else:
      self = other.__list__()

  def __len__(self):
    return ___delta("list-len", self, int)

  def __add__(self, other):
    return ___delta("list+", self, other, list)

  def __list__(self):
    return SeqIter(self).__list__()

  def __iter__(self):
    return SeqIter(self)

  def __tuple__(self):
    return ___delta("list-tuple", self, tuple)

  def __set__(self):
    return ___delta("list-set", self, set)

  def __in__(self, test):
    return ___delta("list-in", self, test)

  def __str__(self):
    return ___delta("list-str", self, str)

  def __getitem__(self, idx):
    return ___delta("list-getitem", self, idx)

  def __setitem__(self, idx, val):
    return ___delta("list-setitem", self, idx, val, list)

  # NOTE(joe): copied code (tuple.py)
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

  def extend(self, other):
    return self.__add__(other)

  def append(self, other):
    self = self.extend([other])

___assign("%list", list)

