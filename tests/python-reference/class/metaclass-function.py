def f(name, bases, dict):
  dict.__delitem__("x")
  return type(name, bases, dict)

x = "global x"
class C(metaclass=f):
  x = "class var"
  def m(self):
    return x

___assertFalse(hasattr(C, x))
___assertEqual(C().m(), "global x")
