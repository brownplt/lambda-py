class str(object):
  def __new__(self):
		return ___delta("str-init", self, dict)

	def __init__(self):
		pass

	def __len(self):
		return ___delta("strlen", self, int)

	def ___str(self):
		return self

	def __add__(self, other):
		return ___delta("str+", self, other, str)

	def __mult__(self, other):
		return ___delta("str*", self, other, str)

	def __eq__(self, other):
		return ___delta("str=", self, other, int)

	def __cmp__(self, other):
		return ___delta("strcmp", self, other, int)

	def __in__(self, test):
		return ___delta("strin", self, test, bool)

	def __min__(self):
		return ___delta("strmin", self, str)

	def __max__(self):
		return ___delta("strmax", self, str)

	def __list__(self):
		return ___delta("strlist", self, list)

	def __tuple__(self):
		return ___delta("str-tuple", self, tuple)

	def __int__(self):
		return ___delta("strint", self, int)

	def __getitem__(self, idx):
		return ___delta("str-getitem", self, idx, str)

	def __slice__(self, lower, upper, step):
		return ___delta("strslice", self, lower, upper step, str)
