class dict(object):
  def __init__(self, *d):
    self.__internaldict__ = []
    if d.__len__() > 0:
      for pair in ___delta("tuple-getitem", d, 0):
        self.__setitem__(pair[0], pair[1])

  def __len__(self):
    result = 0
    for pair in self.__internaldict__:
      result += pair[1].__len__()
    return result

  def __str__(self):
    return self.__internaldict__.__str__()

  def __bool__(self):
    return not ___delta("num=", self.__len__(), 0)

  def __list__(self):
    result = []
    for pair in self.__internaldict__:
      result.extend(pair[1])
    return result

  def get(self, key, *default):
    for pair in self.__internaldict__:
      if key.__hash__().__eq__(pair[0]):
        for elt in pair[1]: 
          if elt[0].__eq__(key):
            return elt[1]

    if default.__len__() == 1:
      return default[0]
    return None

  def __iter__(self):
    return self.keys().__iter__()

  def __in__(self, key):
    for elt in self.__list__():
      if elt[0].__eq__(key):
        return True  
    return False

  def __eq__(self, other):
    if self.__len__() != other.__len__():
      return False
    for x in self:
      if self.__getitem__(x) != other.__getitem__(x):
        return False
    return True

  def clear(self):
    self.__internaldict__ = []

  def update(self, *other):
    if ___delta("num=", other.__len__(), 0):
      pass
    else:
      for pair in other.__getitem__(0).__list__():
        self.__setitem__(pair[0], pair[1])

  def keys(self):
    set = ___id("%set")
    result = []
    for p in self.__list__():
      result.append(p[0]) 
    return set(result)

  def values(self):
    set = ___id("%set")
    result = []
    for p in self.__list__():
      result.append(p[1]) 
    return set(result)

  def items(self):
    set = ___id("%set")
    return set(self.__list__())

  def __getitem__(self, key):
    for pair in self.__internaldict__:
      if key.__hash__().__eq__(pair[0]):
        for elt in pair[1]: 
          if elt[0].__eq__(key):
            return elt[1]
    raise KeyError(key)

  def __setitem__(self, key, val):
    range = ___id("%range")
    found = False
    for elt in self.__internaldict__:
      if key.__hash__() == elt[0]:
        found = True
        found_in_lst = False
        for i in range(0, elt[1].__len__()):
          if elt[1][i][0] == key:
            found_in_lst = True
            elt[1][i] = (key, val)

        if not found_in_lst:
          elt[1].append((key, val))

    if not found:
      self.__internaldict__.append((key.__hash__(), [(key, val)]))

  def __delitem__(self, key):
    range = ___id("%range")
    removed = False
    for pair in self.__internaldict__:
      if key.__hash__().__eq__(pair[0]):
        for x in range(0, pair[1].__len__()):
          if pair[1][x][0].__eq__(key):
            removed = True
            ___delta("list-remove", pair[1], x, list)

    if not removed:
      raise KeyError(key)

___assign("%dict", dict)
