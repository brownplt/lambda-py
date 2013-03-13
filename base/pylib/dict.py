
class dict(object):
  def __init__(self):
    self = ___delta("dict-init", self, dict)

  def __len__(self):
    return ___delta("dict-len", self, int)

  def __str__(self):
    return ___delta("dict-str", self, str)

  def __list__(self):
    return ___delta("dict->list", self, list)

  def get(self, key, *default):
    return ___delta("dict-get", self, key, default)

  def __iter__(self):
    return self.keys().__iter__()

  def __in__(self, other):
    return ___delta("dict-in", self, other)

  def __eq__(self, other):
    return dicteq(self, other)

  def clear(self):
    return ___delta("dict-clear", self)

  def update(self, *other):
    return ___delta("dict-update", self, other)

  def keys(self):
    return ___delta("dict-keys", self, set)

  def values(self):
    return ___delta("dict-values", self, set)

  def items(self):
    return ___delta("dict-items", self, set, tuple)

  def __getitem__(self, key):
    return ___delta("dict-getitem", self, key)

  def __setitem__(self, key, val):
    return ___delta("dict-setitem", self, key, val)

  def __delitem__(self, key):
    return ___delta("dict-delitem", self, key)

___assign("%dict", dict)

