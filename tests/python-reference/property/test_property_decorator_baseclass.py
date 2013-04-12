class BaseClass(object):
    def __init__(self):
        self._spam = 5

    @property
    def spam(self):
        """BaseClass.getter"""
        return self._spam

    @spam.setter
    def spam(self, value):
        self._spam = value

    @spam.deleter
    def spam(self):
        del self._spam

def test_property_decorator_baseclass():
    # see #1620
    base = BaseClass()
    ___assertEqual(base.spam, 5)
    ___assertEqual(base._spam, 5)
    base.spam = 10
    ___assertEqual(base.spam, 10)
    ___assertEqual(base._spam, 10)
    delattr(base, "spam")
    ___assertTrue(not hasattr(base, "spam"))
    ___assertTrue(not hasattr(base, "_spam"))
    base.spam = 20
    ___assertEqual(base.spam, 20)
    ___assertEqual(base._spam, 20)

test_property_decorator_baseclass()
