def f(g):
  g.x = 22
  return g

___assertEqual(f(lambda: None).x, 22)

