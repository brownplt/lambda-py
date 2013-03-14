
class list(object):
  def __new__(cls, *args):
    list = ___id("%list")
    type = ___id("%type")
    if ___delta("num=", args.__len__(), 0):
      # list-init preserves the class pointer of self to support inheritance
      return ___delta("list-init", [], cls)
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
    int = ___id("%int")
    return ___delta("list-len", self, int)

  def __add__(self, other):
    list = ___id("%list")
    return ___delta("list+", self, other, list)

  def __list__(self):
    SeqIter = ___id("%SeqIter")
    return SeqIter(self).__list__()

  def __iter__(self):
    SeqIter = ___id("%SeqIter")
    return SeqIter(self)

  def __tuple__(self):
    tuple = ___id("%tuple")
    return ___delta("list-tuple", self, tuple)

  def __set__(self):
    set = ___id("%set")
    return ___delta("list-set", self, set)

  def __in__(self, test):
    for elt in self:
      if test.__eq__(elt):
        return True
    return False

  def __str__(self):
    str = ___id("%str")
    return ___delta("list-str", self, str)

  def __bool__(self):
    return not ___delta("num=", self.__len__(), 0)

  def __getitem__(self, idx):
    return ___delta("list-getitem", self, idx)

  def __setitem__(self, idx, val):
    list = ___id("%list")
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
    list = ___id("%list")
    type = ___id("%type")
    if (type(other) == list):
        ___delta("list-extend", self, other, self.__class__)
    else:
        ___delta("list-extend", self, other.__list__(), self.__class__)

  def append(self, other):
    self.extend([other])

  def remove(self, other):
    removed = False
    for x in range(0, len(self)):
      if ___delta("list-getitem", self, x) == other:
        removed = True
        ___delta("list-remove", self, x)
    raise ValueError('list.remove(x): x not in list')

___assign("%list", list)
