class O(object):
  def __init__(self):
    self = ['why am i a list now']

o = O()
assert(type(o) != list)

