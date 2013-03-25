class PropertyBase(Exception):
    pass

class PropertyGet(PropertyBase):
    pass

class PropertySet(PropertyBase):
    pass

class PropertyDel(PropertyBase):
    pass

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

class SubClass(BaseClass):

    @BaseClass.spam.getter
    def spam(self):
        """SubClass.getter"""
        raise PropertyGet(self._spam)

    @spam.setter
    def spam(self, value):
        raise PropertySet(self._spam)

    @spam.deleter
    def spam(self):
        raise PropertyDel(self._spam)

class PropertyDocBase(object):
    _spam = 1
    def _get_spam(self):
        return self._spam
    spam = property(_get_spam, doc="spam spam spam")

class PropertyDocSub(PropertyDocBase):
    @PropertyDocBase.spam.getter
    def spam(self):
        """The decorator does not use this doc string"""
        return self._spam

class PropertySubNewGetter(BaseClass):
    @BaseClass.spam.getter
    def spam(self):
        """new docstring"""
        return 5

class PropertyNewGetter(object):
    @property
    def spam(self):
        """original docstring"""
        return 1
    @spam.getter
    def spam(self):
        """new docstring"""
        return 8

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



