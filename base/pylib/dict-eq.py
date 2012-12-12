def dicteq(d1, d2):
  if len(d1) == len(d2):
    for x in d1:
      v = d1[x]
      if d2[x] != v:
        return False
    return True
  else:
    return False

  d = {'a': 1, 'b': 2}
  e = {'a': 1, 'b': 2}
  dicteq(d, e)
