class C(object):
  def __init__(self):
    self.x = 42
  @property
  def f(self):
    self.x += 1
    return self.x

def assign_to_f():
  C().f = 12
___assertRaises(AttributeError, assign_to_f)
