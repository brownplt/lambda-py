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

  # use kw_dict and __defaults__ to fill missing arguments
  n_params = params.__len__()
  defaults = fun.__defaults__
  n_defaults = defaults.__len__()
  result = stararg.__list__()
  i = nargs + stararg.__len__()
  while i < n_params:
    arg = params[i]
    if arg in kw_dict:
      result.append(kw_dict[arg])
      del kw_dict[arg]
    elif n_params - i <= n_defaults:
      result.append(defaults[n_defaults - n_params + i])
    else:
      raise TypeError("missing argument " + arg)
    i += 1

  # TODO: if fun has **kwarg use kw_dict as argument
  if kw_dict != {}:
    raise TypeError("unexpected keyword argument(s): " + kw_dict.__str__())

  return result.__tuple__()

___assign("%call_stararg", ___call_stararg)
