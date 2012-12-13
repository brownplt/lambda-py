def nested_reraise():
    raise
def reraise():
    try:
        raise TypeError("foo")
    except:
        nested_reraise()
___assertRaises(TypeError, reraise)
