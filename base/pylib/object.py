def ___object__getattribute__(obj, key):
    #print("object.__getattribute__"); print(obj); print(key)
    try:
      return ___delta("obj-getattr", obj, key)
    except:
      pass
    obj_cls = ___delta("$class", obj)
    #print(obj_cls)
    val = ___getattr(obj_cls, key)
    #print(val)
    val_cls = ___delta("$class", val)
    #print(val_cls)
    try:
      get = ___getattr(val_cls, "__get__")
      #print(get)
    except:
      return val
    else:
      return get(val, obj, obj_cls)
    
object.__getattribute__ = ___object__getattribute__
