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
    if ___delta("str=", key, "__call__"):
      # the __call__ attribute is the function itself
      return self
    if ___delta("str=", key, "__defaults__"):
      # for functions withoud defaults, they are ()
      return ()
    else:
      str = ___id("%str")
      msg = ___delta("str+", "function object has not attribute ", key, str)
      raise AttributeError(msg)

___assign("%function", function)

# function to compute stararg using defaults and keywords
def ___call_stararg(fun, params, nargs, keywords, stararg, kwarg):
  # collect keyword arguments (first check can be avoided with Python parser)
  kw_dict = {}
  for (k, v) in keywords:
    if k in kw_dict:
      raise SyntaxError("keyword argument repeated")
    kw_dict[k] = v
  if kwarg:
    for k in kwarg:
      if k in kw_dict:
        raise TypeError("multiple values for argument")
      kw_dict[k] = kwarg[k]

  # TODO: use kw_dict to fill missing arguments before __defaults__
  missing_args = params.__len__() - nargs - stararg.__len__()
  n_defaults = fun.__defaults__.__len__()
  result = stararg.__list__()
  if missing_args <= n_defaults:
    for i in range(n_defaults-missing_args, n_defaults):
      result.append(fun.__defaults__[i])
  return result.__tuple__()

___assign("%call_stararg", ___call_stararg)
