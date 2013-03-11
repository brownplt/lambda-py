def f():
  global x
  x = 4
  return "key1"

def g():
  global y
  y = 5
  return "key2"

o = {f(): "fval", g(): "gval"}

___assertEqual(o.keys(), {"key1", "key2"})
___assertEqual(x, 4)
___assertEqual(y, 5)
