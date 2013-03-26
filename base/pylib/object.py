# normal instance attribute lookup: it can be overriden and starts
# looking for the attribute in the object's internal dict.
def ___object__getattribute__(obj, key):
    try:
      # return obj.__dict__[key]
      return ___delta("obj-getattr", obj, key)
    except:
      pass
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
        val = getattr(obj, key)
    val_cls = ___delta("$class", val)
    try:
      get = ___getattr(val_cls, "__get__")
    except:
      return val
    else:
      return get(val, obj, obj_cls)

___assign("%special_getattr", special_getattr)

# the computed obj.__dict__ attribute is a snapshot for now,
# it should be a proxy dict.
def obj_dict(obj):
    list = ___id("%list")
    str = ___id("%str")
    obj_dict = {}
    obj_dir = ___delta("obj-dir", obj, list, str)
    for key in obj_dir:
      if key != "__bases__" and key != "__mro__":
        obj_dict[key] = ___delta("obj-getattr", obj, key)
    return obj_dict

___assign("%obj_dict", obj_dict)

def ___object_setattr__(obj, key, value):
    try:
      # If the field is present on the object, then try
      # to __set__ it if an accessor, otherwise just stick
      # it on the object
      # Perhaps in the future should be
      # obj.__dict__[key]
      obj_cls = ___delta("$class", obj)
      val = ___getattr(obj_cls, key)
      val_cls = ___delta("$class", val)
      try:
        set = ___getattr(val_cls, "__set__")
        set(val, obj, obj_cls, value)
      except:
        ___setattr(obj, key, value)
    except:
      ___setattr(obj, key, value)


object.__setattr__ = ___object_setattr__

