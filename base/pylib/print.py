def print(*args):
  s = ''
  i = len(args)
  for a in args:
    s += a.__str__()
    i = i - 1
    if i > 0:
      s += ' '
  ___prim1("print", s)

___assign("%print", print)
