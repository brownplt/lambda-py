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

def test_property_getter_doc_override():
    newgettersub = PropertySubNewGetter()
    ___assertEqual(newgettersub.spam, 5)
    # TODO(joe): docstrings
    #___assertEqual(newgettersub.__class__.spam.__doc__, "new docstring")
    newgetter = PropertyNewGetter()
    ___assertEqual(newgetter.spam, 8)
    # TODO(joe): docstrings
    #___assertEqual(newgetter.__class__.spam.__doc__, "new docstring")

test_property_getter_doc_override()
