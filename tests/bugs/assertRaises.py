def ___assertRaises(e, f, *args):
  try:
    f(*args)
  except e as the_exn:
    return
  else:
    assert(False)
  assert(False)

def check(x):
    if not x:
        raise KeyError
    else:
        return x

___assertRaises(KeyError, check, 0)
