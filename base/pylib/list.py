
class list(object):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      # list-init preserves the class pointer of self to support inheritance
      return ___delta("list-init", self, list)
    elif ___delta("num=", args.__len__(), 1):
      other = ___delta("tuple-getitem", args, 0)
      if (type(other) == list):
        return ___delta("list-copy", other, list)
      else:
        return other.__list__()
    else:
      raise TypeError("list() takes at most 1 argument")

  def __init__(self, *args):
    pass

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
    for elt in self:
      if test.__eq__(elt):
        return True
    return False

  def __str__(self):
    return ___delta("list-str", self, str)

  def __getitem__(self, idx):
    return ___delta("list-getitem", self, idx)

  def __setitem__(self, idx, val):
    ___delta("list-setitem", self, idx, val, list)

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
    ___delta("list-extend", self, other, self.__class__)

  def append(self, other):
    self.extend([other])

___assign("%list", list)

