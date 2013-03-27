class C(object):
  def __init__(self):
    self.x = 42
  @property
  def f(self):
    self.x += 1
    return self.x
  @f.setter
  def f(self, value):
    self.x = value
  @f.deleter
  def f(self):
    del self.x

c = C()
assert c.x == 42
assert c.f == 43
c.f = 55
assert c.x == 55
assert c.f == 56
del c.f
assert not hasattr(c, 'x')
assert not hasattr(c, 'f')
assert hasattr(C, 'f')
