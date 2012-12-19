# This test, modeled after the original complex-defs,
# makes sure that closed-over lists bound to vararg
# function parameters shadow global definitons of the
# same variable name. 


def makeReturner(*lst):
  def returner():
    return lst
  return returner

lst = (4,5,6)

___assertEqual(makeReturner(1,2,3)(), (1,2,3))

def makeReturner2(**kwargs):
  def returner():
    return kwargs
  return returner

kwargs = (4,5,6)

___assertEqual(makeReturner2(a=11)()['a'], 11)
