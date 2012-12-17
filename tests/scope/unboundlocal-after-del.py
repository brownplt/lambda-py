def errorinouter():
  y = 1
  del y
  print(y)
  def inner():
    return y

def errorininner():
  def inner():
    return y
  y = 1
  del y
  inner()

___assertraises(unboundlocalerror, errorinouter)
___assertraises(nameerror, errorininner)

