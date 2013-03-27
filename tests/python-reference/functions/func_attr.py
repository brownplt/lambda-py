def f():
  return f.x

f.x = 22

___assertEqual(f(), 22)

