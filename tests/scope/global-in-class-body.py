x = 12

class Global:
  global x
  x = 13
  def set(self, val):
    x = val
  def get(self):
    return x

g = Global()
___assertEqual(g.get(), 13)

g.set(15)
___assertEqual(g.get(), 13)
