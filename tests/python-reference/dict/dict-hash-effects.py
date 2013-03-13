hashcalls = []
eqcalls = []
cmpcalls = []

class O():
  def __init__(self, msg):
    self.msg = msg

  def __hash__(self):
    hashcalls.append(self.msg)
    return 5

  def __eq__(self, other):
    eqcalls.append(self.msg + " == " + other.msg)
    return self is other

  def __cmp__(self, other):
    cmpcalls.append(self.msg + " == " + other.msg)
    return id(self) - id(other)

d = {}
o1 = O("o1")
o2 = O("o2")
o3 = O("o3")

def reset():
  global hashcalls, eqcalls, cmpcalls
  eqcalls = []
  hashcalls = []
  cmpcalls = []

d[o1] = "1"
assert(hashcalls == ["o1"])
assert(eqcalls == [])
assert(cmpcalls == [])
reset()

d[o2] = "2"
assert(hashcalls == ["o2"])
assert(eqcalls == ["o1 == o2"])
assert(cmpcalls == [])
reset()

d[o3] = "3"
assert(hashcalls == ["o3"])
assert(eqcalls == ["o1 == o3", "o2 == o3"])
assert(cmpcalls == [])

