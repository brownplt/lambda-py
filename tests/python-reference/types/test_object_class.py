def test_object_class():
    # Testing object class...
    a = object()
    ___assertEqual(a.__class__, object)
    ___assertEqual(type(a), object)
    b = object()
    #___assertNotEqual(a, b) Not defined in python-lib-bindings (Alejandro)
    ___assertTrue(a != b)
    ___assertFalse(hasattr(a, "foo"))
    # NB: we could easily enforce the restriction for assigning attributes to
    # object instances in object.__setattr__ (Alejandro)
    """
    try:
        a.foo = 12
    except (AttributeError, TypeError):
        pass
    else:
        assert False, "object() should not allow setting a foo attribute"
    """
    ___assertFalse(hasattr(object(), "__dict__"))

    class Cdict(object):
        pass
    x = Cdict()
    ___assertEqual(x.__dict__, {})
    x.foo = 1
    ___assertEqual(x.foo, 1)
    ___assertEqual(x.__dict__, {'foo': 1})

test_object_class()
