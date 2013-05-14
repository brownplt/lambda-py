class Meta(type):
  def __new__(self, name, bases, dict):
    dict.__delitem__("x")
    return type.__new__(self, name, bases, dict)

class B(metaclass=Meta):
  x = "class var"
  y = x

x = "global x"
class C(B):
  x = "class var"
  def m(self):
    return x

___assertFalse(hasattr(C, x))
___assertEqual(C.y, "class var")
___assertEqual(C().m(), "global x")
___assertIs(C.__class__, Meta)
