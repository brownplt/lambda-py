class function(object):
  def __get__(self, obj, objtype):
    # when used as attribute a function returns a bound method or the function itself
    method = ___id("%method")
    isinstance = ___id("%isinstance")
    if isinstance(obj, objtype):
        return method(self, obj)
    else:
        return self

___assign("%function", function)
