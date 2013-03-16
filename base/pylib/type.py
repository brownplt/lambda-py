
class type(object):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 1):
      first_arg = ___delta("tuple-getitem", args, 0)
      return ___delta("$class", first_arg)
    elif ___delta("num=", args.__len__(), 3):
      name = ___delta("tuple-getitem", args, 0)
      bases = ___delta("tuple-getitem", args, 1)
      dict = ___delta("tuple-getitem", args, 2)
      cls = ___delta("type-new", name)
      if ___delta("type-uniqbases", bases):
        cls.__bases__ = bases
        try:
          cls.__mro__ = ___delta("type-buildmro", bases)
        except:
          raise TypeError("cannot create a consisten method resolution order")
        else:
          #for k,v in dict.items():
          #  setattr(cls, k, v)
          return cls
      else:
        raise TypeError("duplicate base")
    else:
      raise TypeError("type() takes 1 or 3 arguments")

  def __init__(self, *args):
    pass

  def __call__(cls, *args):
    type = ___id("%type")
    if cls is type and ___delta("num=", args.__len__(), 1):
      first_arg = ___delta("tuple-getitem", args, 0)
      return ___delta("$class", first_arg)
    obj = cls.__new__(cls, *args)
    if isinstance(obj, cls):
      if obj.__init__(*args):
        raise TypeError("__init__() should return None")
    return obj

  def __bool__(self): return True

___assign("%type", type)

