x = 5
class C(object):
  x = 10
  def f(self):
    return x
  y = x + 10

c = C()
___assertEqual(c.y, 20)
___assertEqual(c.f(), 5)
