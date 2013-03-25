class C(object):
  def __init__(self):
    self.x = 42
  @property
  def f(self):
    self.x += 1
    return self.x

c = C()
___assertEqual(c.f, 43)
___assertEqual(c.f, 44)
___assertEqual(c.f, 45)

