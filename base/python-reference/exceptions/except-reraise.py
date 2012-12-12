def reraise():
    try:
        raise TypeError("foo")
    except:
        try:
            raise KeyError("caught")
        except KeyError:
            pass
        raise
___assertRaises(TypeError, reraise)
