class NoneType(object):
  def __new__(self, *args):
    return None

  def __init__(self, *args):
    pass

  def __bool__(self):
    return False

  def __str__(self):
    return "None"

___assign("%NoneType", NoneType)
