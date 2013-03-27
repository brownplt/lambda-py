x = 12

# global variable x is not an instance variable, it's 
# the global value x = 12. However, x inside of the 
# method set becomes local when set. However, if it is
# not set locally (as is the case in get), the global
# value is returned instead. 
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
