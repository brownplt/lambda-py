shadowed = False
def property(f):
  global shadowed
  shadowed = True
  return f

class C(object):
  @property
  def meth(self):
    pass

C()
___assertTrue(shadowed)

