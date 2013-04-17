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
    elif ___delta("str=", key, "__name__"):
      # default name
      return ""
    elif ___delta("str=", key, "__doc__"):
      # default docstring
      return ""
    elif ___delta("str=", key, "___nkwonlyargs"):
      # for functions without keyword-only args
      return 0
    elif ___delta("str=", key, "___nkwarg"):
      # for functions without kwarg
      return 0
    elif ___delta("str=", key, "__defaults__"):
      # for functions without defaults
      return ()
    elif ___delta("str=", key, "__kwdefaults__"):
      # for functions without kwdefaults
      return ()
    else:
      str = ___id("%str")
      msg = ___delta("str+", "function object has not attribute ", key, str)
      raise AttributeError(msg)

___assign("%function", function)

# function to compute positional arguments list according to
# http://docs.python.org/3.2/reference/expressions.html#calls
def ___call_stararg(fun, params, args, keywords, stararg, kwarg):

  # collect keyword arguments (first check can be avoided with Python parser)
  kw_dict = {}
  for (k, v) in keywords:
    if k in kw_dict:
      raise SyntaxError("keyword argument repeated: " + k)
    kw_dict[k] = v
  if kwarg:
    for k in kwarg:
      if k in kw_dict:
        raise TypeError("multiple values for argument: " + k)
      kw_dict[k] = kwarg[k]

  # parameters
  n_params = params.__len__() # total number of parameters
  n_kwonly = fun.___nkwonlyargs # number of keyword-only paramters
  n_kwarg = fun.___nkwarg # number of kwarg (0 or 1)
  n_pospar = n_params - n_kwonly - n_kwarg # number of positional parameters

  # positional arguments
  pos_args = args + stararg
  n_posarg = pos_args.__len__()

  # defaults
  defaults = fun.__defaults__
  n_defaults = defaults.__len__()
  kwdefaults = fun.__kwdefaults__
  n_kwdefaults = kwdefaults.__len__()

  result = []
  i = 0
  # for positional parameters use positional arguments, then keywords, then defaults
  while i < n_pospar:
    arg = params[i]
    if i < n_posarg:
      result.append(pos_args[i])
    elif arg in kw_dict:
      result.append(kw_dict[arg])
      del kw_dict[arg]
    elif n_pospar - i <= n_defaults:
      result.append(defaults[n_defaults - n_pospar + i])
    else:
      raise TypeError("missing argument " + arg)
    i += 1

  # for keyword-only parameters use keywords, then defaults
  while i < n_pospar + n_kwonly:
    arg = params[i]
    if arg in kw_dict:
      result.append(kw_dict[arg])
      del kw_dict[arg]
    elif n_pospar + n_kwonly - i <= n_kwdefaults:
      result.append(kwdefaults[n_kwdefaults - n_pospar - n_kwonly + i])
    else:
      raise TypeError("missing keyword-only argument " + arg)
    i += 1

  # for **kwarg use kw_dict but, raise TypeError if not empty
  if n_kwarg > 0:
    result.append(kw_dict)
  elif kw_dict != {}:
    raise TypeError("unexpected keyword argument(s): " + kw_dict.__str__())

  # remaining positional arguments go to vararg
  i = n_pospar
  while i < n_posarg:
    result.append(pos_args[i])
    i += 1

  return result.__tuple__()

___assign("%call_stararg", ___call_stararg)
