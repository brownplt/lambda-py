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
    return ___delta("strlen", self, int)

  def __str__(self):
    return self

  def __add__(self, other):
    return ___delta("str+", self, other, str)

  def __mult__(self, other):
    return ___delta("str*", self, other, str)

  def __iter__(self):
    return SeqIter(self)

  def __eq__(self, other):
    return ___delta("str=", self, other)

  def __cmp__(self, other):
    return ___delta("strcmp", self, other, int)

  def __in__(self, test):
    return ___delta("strin", self, test)

  def __min__(self):
    return ___delta("strmin", self, str)

  def __max__(self):
    return ___delta("strmax", self, str)

  def __list__(self):
    l = ___delta("strlen", self, int)
    return [self[i] for i in range(0, l)]

  def __tuple__(self):
    return tuple(self.__list__())

  def __int__(self):
    return ___delta("strint", self, int)

  def __getitem__(self, idx):
    return ___delta("str-getitem", self, idx, str)

  def __slice__(self, lower, upper, step):
    return ___delta("strslice", self, lower, upper, step, str)

___assign("%str", str)
