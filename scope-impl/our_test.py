
class C:
  x=1
  @classmethod
  def f(cl, y):
    return cl.x
func=C.f
print(func(1))
