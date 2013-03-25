def ___assertEqual(l, r):
  assert(l == r)

def ___assertNotEqual(l, r):
  assert(not (l == r))

def ___assertTrue(v):
  assert(v)

def ___assertFalse(v):
  assert(not v)

def ___assertIs(l, r):
  assert(l is r)

def ___assertIsNot(l, r):
  assert(not (l is r))

def ___assertIn(l, r):
  assert(l in r)

def ___assertNotIn(l, r):
  assert(not (l in r))

def ___assertRaises(e, f, *args):
  try:
    f(*args)
  except e as the_exn:
    return
  else:
    assert(False)
  assert(False)

def ___fail():
  assert(False)

