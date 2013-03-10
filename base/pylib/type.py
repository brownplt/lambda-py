
class type(object):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 1):
      first_arg = ___delta("tuple-getitem", args, 0)
      return ___delta("$class", first_arg)
    else:
      raise TypeError("Only type(obj) form is supported")

  def __init__(self, *args):
    pass

  def __call__(cls, *args):
    obj = cls.__new__(cls, *args)
    if isinstance(obj, cls):
      if obj.__init__(*args):
        raise TypeError("__init__() should return None")
    return obj

  def __bool__(self): return True

___assign("%type", type)

