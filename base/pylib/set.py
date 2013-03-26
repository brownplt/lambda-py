
class set(object):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      return ___emptyset()
    else:
      result = []
      for elt in args.__getitem__(0):
        if not result.__in__(elt):
          result.append(elt)
      return result.__set__()
      #return args.__getitem__(0).__set__()

  def __init__(self, *args):
    pass

  def __len__(self):
    int = ___id("%int")
    return ___delta("set-len", self, int)

  def __set__(self):
    # NOTE(joe): list copy can do the job here, no need for set-copy prim
    return self.__list__().__set__()

  def __list__(self):
    list = ___id("%list")
    return ___delta("set-list", self, list)

  def __str__(self):
    str = ___id("%str")
    return ___delta("set-str", self, str)

  def __bool__(self):
    return not ___delta("num=", self.__len__(), 0)

  def __iter__(self):
    SeqIter = ___id("%SeqIter")
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

    return result.__set__()

  def __and__(self, other):
    result = []
    for elt in self.__list__():
      if other.__in__(elt):
        result.append(elt)

    return result.__set__()

  def __or__(self, other):
    result = []
    for elt in self.__list__():
      # skip it the first time
      if not other.__in__(elt):
        result.append(elt)

    for elt in other.__list__():
      result.append(elt)

    return result.__set__()

  def __xor__(self, other):
    result = []
    for elt in self.__list__():
      if not other.__in__(elt):
        result.append(elt)

    for elt in other.__list__():
      if not self.__in__(elt):
        result.append(elt)

    return result.__set__()

___assign("%set", set)
