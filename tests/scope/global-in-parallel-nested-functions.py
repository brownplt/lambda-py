def f():
  y = 1
  def g():
    global y
    return y
  def h():
    return y + 1
  return g, h

y = 9
g, h = f()
___assertEqual(g(), 9)
___assertEqual(h(), 2)
