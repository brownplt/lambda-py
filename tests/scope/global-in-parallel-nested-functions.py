# h closes over the variable y, and so returns it with
# the value it had locally when declared. However, g
# declares y as a global variable, which then picks up
# the top-level definition y = 9. 

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
