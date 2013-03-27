# method type
# represents a bound method
class method(object):
  def __init__(self, func, obj):
    # __func__ is the underlying function and __self__ the bound object
    self.__func__ = func
    self.__self__ = obj

  def __getattr__(self, key):
    # the __call__ attribute is the method itself
    if ___delta("str=", key, "__call__"):
      return self
    else:
      str = ___id("%str")
      msg = ___delta("str+", "method object has not attribute ", key, str)
      raise AttributeError(msg)

  def __str__(self):
    # we don't have __name__ for functions and classes, yet.
    return "(method of " + str(type(self.__self__)) + " object)"

___assign("%method", method)

# classmethod type
# classmethod objects are converted to method objects 
# with class as __self__ on attribute retrieval
class classmethod(object):
  def __init__(self, func):
    # __func__ is the underlying function
    self.__func__ = func

  def __get__(self, obj, objtype):
    # when used as attribute classmethod returns a method bound to the class
    object = ___id("%object")
    method = ___id("%method")
    new_method = object.__new__(method)
    method.__init__(new_method, self.__func__, objtype)
    return new_method

___assign("%classmethod", classmethod)

# staticmethod type
# staticmethod objects are converted to functions on attribute retrieval
class staticmethod(object):
  def __init__(self, func):
    # __func__ is the underlying function
    self.__func__ = func

  def __get__(self, obj, objtype):
    # when used as attribute classmethod returns the function
    return self.__func__

___assign("%staticmethod", staticmethod)
