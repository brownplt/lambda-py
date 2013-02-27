
class type(object):
  def __init__(self, *args):
    if ___delta("num=", args.__len__(), 1):
      first_arg = ___delta("tuple-getitem", args, 0)
      self = ___delta("$class", first_arg)
    else:
      raise TypeError("Only type(obj) form is supported")

  def __call__(cls, *args):
    obj = cls.__new__(cls, *args)
    if isinstance(obj, cls):
      if obj.__init__(*args):
        raise TypeError("__init__() should return None")
    return obj

___assign("%type", type)

