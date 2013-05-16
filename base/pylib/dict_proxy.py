class dict_proxy(dict):
  def __init__(self, obj):
    type = ___id("%type")
    isinstance = ___id("%isinstance")
    self.obj = obj
    self.is_class = isinstance(self.obj, type)

  def __list__(self):
    list = ___id("%list")
    str = ___id("%str")
    obj_dir = ___delta("obj-dir", self.obj, list, str)
    result = []
    for key in obj_dir:
      if not self.is_class or (key != "__bases__" and key != "__mro__"):
        result.append(key)
    return result

  def __itemslist__(self):
    result = []
    for key in self.__list__():
      val = ___delta("obj-getattr", self.obj, key)
      result.append((key, val))
    return result

  def __getitem__(self, key):
    try:
      return ___delta("obj-getattr", self.obj, key)
    except:
      raise KeyError(key)

  def __setitem__(self, key, val):
    if self.is_class:
      raise TypeError("'dict_proxy' object does not support item assignment")
    setattr = ___id("%setattr")
    setattr(self.obj, key, val)

  def __delitem__(self, key):
    if self.is_class:
      raise TypeError("'dict_proxy' object does not support item deletion")
    delattr = ___id("%delattr")
    delattr(self.obj, key)


___assign("%dict_proxy", dict_proxy)
