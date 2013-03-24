class str(object):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      return ""
    else:
      first_arg = ___delta("tuple-getitem", args, 0)
      return first_arg.__str__()

  def __init__(self, *args):
    pass

  def __len__(self):
    int = ___id("%int")
    return ___delta("strlen", self, int)

  def __str__(self):
    return self

  def __add__(self, other):
    str = ___id("%str")
    return ___delta("str+", self, other, str)

  def __mult__(self, other):
    str = ___id("%str")
    return ___delta("str*", self, other, str)

  def __iter__(self):
    SeqIter = ___id("%SeqIter")
    return SeqIter(self)

  def __eq__(self, other):
    type = ___id("%type")
    str = ___id("%str")
    if not (type(other) is str):
      return False
    return ___delta("str=", self, other)

  def __hash__(self):
    int = ___id("%int")
    return ___delta("str-hash", self, int)

  def __cmp__(self, other):
    int = ___id("%int")
    return ___delta("strcmp", self, other, int)

  def __in__(self, test):
    return ___delta("strin", self, test)

  def __min__(self):
    str = ___id("%str")
    return ___delta("strmin", self, str)

  def __max__(self):
    str = ___id("%str")
    return ___delta("strmax", self, str)

  def __list__(self):
    int = ___id("%int")
    range = ___id("%range")
    l = ___delta("strlen", self, int)
    return [self[i] for i in range(0, l)]

  def __tuple__(self):
    tuple = ___id("%tuple")
    return tuple(self.__list__())

  def __int__(self):
    int = ___id("%int")
    return ___delta("strint", self, int)

  def __bool__(self):
    return self.__len__() != 0

  def __getitem__(self, idx):
    str = ___id("%str")
    return ___delta("str-getitem", self, idx, str)

  def __slice__(self, lower, upper, step):
    str = ___id("%str")
    return ___delta("strslice", self, lower, upper, step, str)

___assign("%str", str)
