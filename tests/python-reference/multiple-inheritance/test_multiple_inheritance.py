# Testing multiple inheritance...
class C(object):
    def __init__(self):
        self.__state = 0
    def getstate(self):
        return self.__state
    def setstate(self, state):
        self.__state = state
a = C()
___assertEqual(a.getstate(), 0)
a.setstate(10)
___assertEqual(a.getstate(), 10)
class D(dict, C):
    def __init__(self):
        type({}).__init__(self)
        C.__init__(self)
d = D()
___assertEqual(list(d.keys()), [])
d["hello"] = "world"
___assertEqual(list(d.items()), [("hello", "world")])
___assertEqual(d["hello"], "world")
___assertEqual(d.getstate(), 0)
d.setstate(10)
___assertEqual(d.getstate(), 10)
___assertEqual(D.__mro__, (D, dict, C, object))

# SF bug #442833
class Node(object):
    def __int__(self):
        return int(self.foo())
    def foo(self):
        return "23"
class Frag(Node, list):
    def foo(self):
        return "42"
___assertEqual(Node().__int__(), 23)
___assertEqual(int(Node()), 23)
___assertEqual(Frag().__int__(), 42)
___assertEqual(int(Frag()), 42)
___assertEqual(Frag.__mro__, (Frag, Node, list, object))

