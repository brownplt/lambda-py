class C:
  a = 1
  
d = C.__dict__
___assertEqual(d['a'], 1)

C.b = 2
___assertEqual(d['b'], 2)

try:
  d['b'] = 5
except TypeError:
  pass
else:
  ___fail()

try:
  del d['b']
except TypeError:
  pass
else:
  ___fail()

del C.b
___assertFalse('b' in d)