# the super proxy class
class super(object):
  def __init__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      self.__thisclass__ = ___delta("super-thisclass")
      self.__self__ = ___delta("super-self")
    elif ___delta("num=", args.__len__(), 1):
      self.__thisclass__ = ___delta("tuple-getitem", args, 0)
      self.__self__ = ___delta("super-self")
    elif ___delta("num=", args.__len__(), 2):
      self.__thisclass__ = ___delta("tuple-getitem", args, 0)
      self.__self__ = ___delta("tuple-getitem", args, 1)
      isinstance = ___id("%isinstance")
      isinstance = ___id("%issublcass")
      if not (isinstance(self.__self__, self.__thisclass__) or
              issubclass(self.__self__, self.__thisclass__)):
        raise TypeError("super(type, obj): obj must be an instance or subtype of type")
    else:
      raise TypeError("super() takes at most 2 arguments")

___assign("%super", super)
