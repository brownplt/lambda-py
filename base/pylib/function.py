# function type
class function(object):
  def __get__(self, obj, objtype):
    # when used as attribute a function returns a bound method or the function itself
    object = ___id("%object")
    method = ___id("%method")
    NoneType = ___id("%NoneType")
    if obj is None and objtype is not NoneType:
         # no object to bind, result is the function itself
        return self
    else:
        # result is a method bound to obj with self as the underlying function
        new_method = object.__new__(method)
        method.__init__(new_method, self, obj)
        return new_method

  def __getattr__(self, key):
    # the __call__ attribute is the function itself
    if ___delta("str=", key, "__call__"):
      return self
    else:
      str = ___id("%str")
      msg = ___delta("str+", "function object has not attribute ", key, str)
      raise AttributeError(msg)

___assign("%function", function)
