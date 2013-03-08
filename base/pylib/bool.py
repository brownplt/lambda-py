class bool(int):
  def __new__(self, *args):
    if ___delta("num=", args.__len__(), 0):
      return False
    else:
      return args.__getitem__(0).__bool__()

  def __init__(self, *args):
    pass

  def __str__(self):
    if self == True:
      return "True"
    return "False"

  def __int__(self):
    return self + 0

  def __float__(self):
    return self + 0.0

___assign("%bool", bool)
