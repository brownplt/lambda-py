# This will be overridden by str.py
___assign("%str", None)

def __setattr__(cls, key, val):
  ___setattr(cls, key, val)

class type(object):
  ___assign("%type", type)
  ___setattr(type, "__setattr__", __setattr__)

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
          cls.__mro__ = ___delta("type-buildmro", (cls,), bases)
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

  def __init__(cls, *args):
    pass

  def __call__(cls, *args):
    type = ___id("%type")
    if cls is type and ___delta("num=", args.__len__(), 1):
      first_arg = ___delta("tuple-getitem", args, 0)
      return ___delta("$class", first_arg)
    obj = cls.__new__(cls, *args)
    if ___delta("isinstance", obj, cls):
      if cls.__init__(obj, *args):
        raise TypeError("__init__() should return None")
    return obj

  def __bool__(cls): return True

  def __getattribute__(cls, key):
    val = ___getattr(cls, key)
    val_cls = ___delta("$class", val)
    try:
      get = ___getattr(val_cls, "__get__")
    except:
      return val
    else:
      return get(val, None, cls)

  def __dir__(cls):
    list = ___id("%list")
    set = ___id("%set")
    result = []
    for c in cls.__mro__:
        c_dir = ___delta("obj-dir", c, list, str)
        result.extend(c_dir)
    return list(set(result))
