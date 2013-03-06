# the super proxy class
class super(object):
  def __init__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      self.__thisclass__ = ___delta("super-thisclass")
      self.__self__ = ___delta("super-self")
    else:
      raise TypeError("super() only the no arguments version is supported")

  def __str__(self):
    return "(super: " + str(self.__thisclass__) + ", " + str(self.__self__) + ")"
