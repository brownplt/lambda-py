# method type
# In method objects __func__ and __self__ are defined as instance attributes
# __call__ attribute is special cased to be the method object itself
class method(object):
  def __init__(self, func, obj):
    self.__func__ = func
    self.__self__ = obj

  def __str__(self):
    # we don't have __name__ for functions and classes, yet.
    return "(method of " + str(type(self.__self__)) + " object)"

  def __get__(self, obj, objtype):
    # when used as attribute a bound method returns the method itself or the associated function
    method = ___id("%method")
    isinstance = ___id("%isinstance")
    if isinstance(obj, objtype):
        return self
    else:
        return self.__func__

___assign("%method", method)

# classmethod type
# In classmethod objects __func__ is defined as instance attribute
# classmethod objects are converted to method objects 
# with class as __self__ on attribute retrieval
class classmethod(object):
  def __init__(self, func):
    self.__func__ = func

  def __get__(self, obj, objtype):
    # when used as attribute classmethod returns a method bound to the class
    return method(self.__func__, objtype)

___assign("%classmethod", classmethod)

# staticmethod type
# In staticmethod objects __func__ is defined as instance attribute
# staticmethod objects are converted to functions on attribute retrieval
class staticmethod(object):
  def __init__(self, func):
    self.__func__ = func

  def __get__(self, obj, objtype):
    # when used as attribute classmethod returns the function
    return self.__func__

___assign("%staticmethod", staticmethod)
