def f(a, b=2, c=3):
    return (a, b, c)

# duplicate keyword
try:
  f(1, a=11, b=22)
except TypeError:
  pass
else:
  ___fail()

try:
  f(1, **{'a':11, 'c':33})
except TypeError:
  pass
else:
  ___fail()

# too much keywords without **kwarg
try:
  f(1, b=22, c=33, d=44)
except TypeError:
  pass
else:
  ___fail()

try:
  f(1, **{'b':22, 'c':33, 'd':44})
except TypeError:
  pass
else:
  ___fail()

# missing argument
try:
  f(b=22, c=33)
except TypeError:
  pass
else:
  ___fail()

try:
  f(**{'b':22, 'c':33})
except TypeError:
  pass
else:
  ___fail()
