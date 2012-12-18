def makeReturner(*lst):
  def returner():
    return lst
  return returner

def makeReturner2(**kwargs):
  def returner():
    return kwargs
  return returner

___assertEqual(makeReturner(1,2,3)(), (1,2,3))
___assertEqual(makeReturner2(a=11)()['a'], 11)
