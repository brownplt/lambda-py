# This will be overridden by str.py
___assign("%str", None)

def __setattr__(cls, key, val):
  if ___delta("str=", key, "__dict__"):
    raise AttributeError("attribute '__dict__' of 'type' objects is not writable")
  else:
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
      if bases.__len__() == 0:
        bases = (___id("%object"),)
      dict = ___delta("tuple-getitem", args, 2)
      cls = ___delta("type-new", name, self)
      try:
        cls.__bases__ = ___delta("type-uniqbases", bases)
        try:
          cls.__mro__ = ___delta("type-buildmro", (cls,), bases)
        except:
          raise TypeError("cannot create a consisten method resolution order")
        else:
          for k,v in dict.items():
            setattr(cls, k, v)
          return cls
      except:
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
    try:
      val = ___getattr(cls, key)
      val_cls = ___delta("$class", val)
      try:
        get = ___getattr(val_cls, "__get__")
      except:
        return val
      else:
        return get(val, None, cls)
    except:
      metaclass = ___delta("$class", cls)
      val = ___getattr(metaclass, key) 
      val_cls = ___delta("$class", val)
      try:
        get = ___getattr(val_cls, "__get__")
      except:
        return val
      else:
        return get(val, cls, metaclass)

  def __dir__(cls):
    list = ___id("%list")
    set = ___id("%set")
    result = []
    for c in cls.__mro__:
        c_dir = ___delta("obj-dir", c, list, str)
        result.extend(c_dir)
    return list(set(result))

  def __delattr__(obj, key):
    if ___delta("str=", key, "__dict__"):
      raise AttributeError("attribute '__dict__' of 'type' objects is not writable")
    else:
      object = ___id("%object")
      object.__delattr__(obj, key)


# function to determine and call metaclass.
def ___call_metaclass(name, bases, cls, keywords, stararg, kwarg):

  # collect keyword arguments (first check can be avoided with Python parser)
  kw_dict = {}
  for (k, v) in keywords:
    if k in kw_dict:
      raise SyntaxError("class with keyword argument repeated: " + k)
    kw_dict[k] = v
  if kwarg:
    for k in kwarg:
      if k in kw_dict:
        raise TypeError("class with multiple values for argument: " + k)
      kw_dict[k] = kwarg[k]

  if 'metaclass' in kw_dict:
    metaclass = kw_dict['metaclass']
    del kw_dict['metaclass']
  else:
    # TODO: metaclass determination should be revised in this case
    metaclass = bases[0].__class__

  prepare = getattr(metaclass, '__prepare__', None)
  if prepare:
    dct = prepare(name, bases, *stararg, **kw_dict)
  else:
    dct = {}
  # TODO: attribute order should be preserved
  dct.update(cls.__dict__)
  
  callable = ___id("%callable")
  if callable(metaclass):
    return metaclass(name, bases, dct, *stararg, **kw_dict)
  else:
    raise TypeError("metaclass is not callable")

___assign("%call_metaclass", ___call_metaclass)
