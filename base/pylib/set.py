
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
    return self.__list__().__in__(elt)

  def __eq__(self, other):
    if self.__len__() != other.__len__():
      return False
    for elt in self.__list__():
      if not other.__in__(elt):
        return False
    return True

  def __sub__(self, other):
    result = []
    for elt in self.__list__():
      if not other.__in__(elt):
        result.append(elt)

    return set(result)

  def __and__(self, other):
    result = []
    for elt in self.__list__():
      if other.__in__(elt):
        result.append(elt)

    return set(result)

  def __or__(self, other):
    result = []
    for elt in self.__list__():
      # skip it the first time
      if not other.__in__(elt):
        result.append(elt)

    for elt in other.__list__():
      result.append(elt)

    return set(result)

  def __xor__(self, other):
    result = []
    for elt in self.__list__():
      if not other.__in__(elt):
        result.append(elt)

    for elt in other.__list__():
      if not self.__in__(elt):
        result.append(elt)

    return set(result)

___assign("%set", set)

