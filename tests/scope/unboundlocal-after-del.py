def errorInOuter():
  y = 1
  del y
  print(y)
  def inner():
    return y

def errorInInner():
  def inner():
    return y
  y = 1
  del y
  inner()

___assertRaises(UnboundLocalError, errorInOuter)
___assertRaises(NameError, errorInInner)

