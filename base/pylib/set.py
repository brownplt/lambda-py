
class set(object):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      return ___emptyset()
    else:
      return args.__getitem__(0).__set__()

  def __init__(self, *args):
    pass

  def __len__(self):
    return ___delta("set-len", self, int)

  def __set__(self):
    # NOTE(joe): list copy can do the job here, no need for set-copy prim
    return self.__list__().__set__()

  def __list__(self):
    return ___delta("set-list", self, list)

  def __iter__(self):
    return SeqIter(self.__list__())

  def __in__(self, elt):
    return ___delta("set-in", self, elt)

  def __eq__(self, other):
    return ___delta("set-eq", self, other)

  def __sub__(self, other):
    return ___delta("set-sub", self, other, set)

  def __and__(self, other):
    return ___delta("set-and", self, other, set)

  def __or__(self, other):
    return ___delta("set-or", self, other, set)

  def __xor__(self, other):
    return ___delta("set-xor", self, other, set)

___assign("%set", set)

