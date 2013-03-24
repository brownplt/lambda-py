# the super proxy class
class super(object):
  def __init__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      self.__thisclass__ = ___delta("super-thisclass")
      self.__self__ = ___delta("super-self")
    elif ___delta("num=", args.__len__(), 1):
      self.__thisclass__ = ___delta("tuple-getitem", args, 0)
      self.__self__ = None
    elif ___delta("num=", args.__len__(), 2):
      self.__thisclass__ = ___delta("tuple-getitem", args, 0)
      self.__self__ = ___delta("tuple-getitem", args, 1)
      isinstance = ___id("%isinstance")
      issubclass = ___id("%issubclass")
      if not (isinstance(self.__self__, self.__thisclass__) or
              issubclass(self.__self__, self.__thisclass__)):
        raise TypeError("super(type, obj): obj must be an instance or subtype of type")
    else:
      raise TypeError("super() takes at most 2 arguments")

  def __get__(self, obj, objtype):
    if self.__obj__ is None and obj is not None:
      return super(self.__type__, obj)
    else:
      return self

  def __getattr__(self, attr):
    if ___delta("isinstance", self.__self__, self.__thisclass__):
      starttype = ___delta("$class", self.__self__)
    else:
      starttype = self.__self__
    mro = iter(starttype.__mro__)
    for cls in mro:
      if cls is self.__thisclass__:
        break
    # Note: mro is an iterator, so the second loop
    # picks up where the first one left off!
    for cls in mro:
      try:
        x = ___delta("obj-getattr", cls, attr)
        if hasattr(x, "__get__"):
          if ___delta("isinstance", self.__self__, type):
            x = x.__get__(None, self.__self__)
          else:
            self_cls = ___delta("$class", self.__self__)
            x = x.__get__(self.__self__, self_cls)
        return x
      except:
        pass
    str = ___id("%str")
    msg = ___delta("str+", "(super) object has not attribute ", attr, str)
    raise AttributeError(msg)

___assign("%super", super)
