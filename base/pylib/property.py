class property(object):
  def __init__(self, *args):
    int = ___id("%int")
    length = ___delta("tuple-len", args, int)
    if length > 0:
      self.fget = args[0]
    if length > 1:
      self.fset = args[1]
    if length > 2:
      self.fdel = args[2]

  def __get__(self, obj, obj_cls):
    return self.fget(obj)

  def __set__(self, obj, obj_cls, value):
    self.fset(obj, value)

  def __del__(self, idx):
    self.fdel(self, idx)

  def getter(self, g):
    self.fget = g

  def setter(self, s):
    self.fset = s

  def deleter(self, d):
    self.fdel = d

