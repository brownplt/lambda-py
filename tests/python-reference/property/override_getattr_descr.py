class Descriptor(object):
  def __get__(self, obj, objtype):
    def f(name):
      return 'got ' + name
    return f

class B(object):
  __getattr__ = Descriptor()

assert(getattr(B(), "key") == "got key")
