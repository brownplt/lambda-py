# normal instance attribute lookup: it can be overriden and starts
# looking for the attribute in the object's internal dict.
def ___object__getattribute__(obj, key):
    if ___delta("obj-hasattr", obj, key):
      # internal dict has precedence
      return ___delta("obj-getattr", obj, key)
    else:
      # class lookup is common with special attributes
      special_getattr = ___id("%special_getattr")
      return special_getattr(obj, key)

object.__getattribute__ = ___object__getattribute__

# special attribute lookup: it cannot be overriden and it doesn't look
# in the object's internal dict, the lookup chain starts at the class.
def special_getattr(obj, key):
    obj_cls = ___delta("$class", obj)
    try:
      val = ___getattr(obj_cls, key)
    except:
      try:
        getattr = obj_cls.__getattr__
      except:
        str = ___id("%str")
        msg = ___delta("str+", "object has no attribute ", key, str)
        raise AttributeError(msg)
      else:
        # method class is special cased for bootstraping method calls
        if obj_cls is method:
          return getattr(obj, key)
        else:
          return obj.__getattr__(key)
    else:
      val_cls = ___delta("$class", val)
      try:
        get = ___getattr(val_cls, "__get__")
      except:
        return val
      else:
        return get(val, obj, obj_cls)

___assign("%special_getattr", special_getattr)

def ___object_setattr__(obj, key, value):
    obj_cls = ___delta("$class", obj)
    if obj_cls is ___id("%object"):
      str = ___id("%str")
      msg = ___delta("str+", "'object' object has no attribute ", key, str)
      raise AttributeError(msg)
    if ___delta("str=", key, "__dict__"):
      obj.__dict__.clear()
      obj.__dict__.update(value)
      return
    try:
      # If the field is present on the object, then try
      # to __set__ it if an accessor, otherwise just stick
      # it on the object
      # Perhaps in the future should be
      # obj.__dict__[key]
      val = ___getattr(obj_cls, key)
    except:
      pass # key is not in class hierarchy
    else:
      val_cls = ___delta("$class", val)
      try:
        set = ___getattr(val_cls, "__set__")
      except:
        pass # val has no setter
      else:
        set(val, obj, value)
        return
    # if there is no setter, set key to value in the object dict
    ___setattr(obj, key, value)

object.__setattr__ = ___object_setattr__

def ___object__delattr__(obj, key):
    obj_cls = ___delta("$class", obj)
    if obj_cls is ___id("%object"):
      str = ___id("%str")
      msg = ___delta("str+", "'object' object has no attribute ", key, str)
      raise AttributeError(msg)
    if ___delta("str=", key, "__dict__"):
      obj.__dict__.clear()
      return
    try:
      val = ___getattr(obj_cls, key)
    except:
      pass # key is not in class hierarchy
    else:
      val_cls = ___delta("$class", val)
      try:
        delete = ___getattr(val_cls, "__delete__")
      except:
        pass # val has no deleter
      else:
        delete(val, obj)
        return
    # if there is no deleter, try to delete from the object dict
    if ___delta("obj-hasattr", obj, key):
      ___delta("obj-delattr", obj, key)
    else:
      raise AttributeError(key)

object.__delattr__ = ___object__delattr__

def ___object__dir__(obj):
    list = ___id("%list")
    set = ___id("%set")
    result = ___delta("obj-dir", obj, list, str)
    for cls in type(obj).__mro__:
        cls_dir = ___delta("obj-dir", cls, list, str)
        result.extend(cls_dir)
    return list(set(result))

object.__dir__ = ___object__dir__
