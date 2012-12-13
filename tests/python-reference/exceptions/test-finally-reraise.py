def reraise():
    try:
        raise TypeError("foo")
    except:
        try:
            raise KeyError("caught")
        finally:
            raise
___assertRaises(KeyError, reraise)
