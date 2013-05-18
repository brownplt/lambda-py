class C:
  a = 1

# a is on initial C.__dict__
d = C.__dict__
___assertEqual(d['a'], 1)

# C.__dict__ is dinamically updated
C.b = 2
___assertEqual(d['b'], 2)

# 'dict_proxy' object does not support item assignment
try:
  d['b'] = 5
except TypeError:
  pass
else:
  ___fail()

# 'dict_proxy' object does not support item deletion
try:
  del d['b']
except TypeError:
  pass
else:
  ___fail()

del C.b
___assertFalse('b' in d)

# attribute '__dict__' of 'type' objects is not writable
try:
  C.__dict__ = {'x':3}
except AttributeError:
  pass
else:
  ___fail()

# attribute '__dict__' of 'type' objects is not writable
try:
  del C.__dict__
except AttributeError:
  pass
else:
  ___fail()
