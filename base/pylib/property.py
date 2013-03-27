class property(object):
  def __init__(self, *args):
    int = ___id("%int")
    length = ___delta("tuple-len", args, int)
    self.fget = None
    self.fset = None
    self.fdel = None
    if length > 0:
      self.fget = args[0]
    if length > 1:
      self.fset = args[1]
    if length > 2:
      self.fdel = args[2]

  def __get__(self, obj, obj_cls):
    if obj is None:
      return self
    if self.fget is None:
      raise AttributeError("Unreadable attribute")
    return self.fget(obj)

  def __set__(self, obj, value):
    if self.fset is None:
      raise AttributeError("Can't set attribute")
    self.fset(obj, value)

  def __delete__(self, obj):
    if self.fdel is None:
      raise AttributeError("Can't delete attribute")
    self.fdel(obj)

  def getter(self, g):
    self.fget = g
    return self

  def setter(self, s):
    self.fset = s
    return self

  def deleter(self, d):
    self.fdel = d
    return self
