# getattr(object, name[, default])
# Return the value of the named attribute of object. name must be a string.
# If the string is the name of one of the object's attributes, the result is
# the value of that attribute. If the named attribute does not exist, default
# is returned if provided, otherwise AttributeError is raised.
def getattr(obj, name, *args):
    type = ___id("%type")
    if ___delta("num=", args.__len__(), 0):
        if ___delta('$class', obj) is type:
            return type.__getattribute__(obj, name)
        else:
            return obj.__getattribute__(name)
    elif ___delta("num=", args.__len__(), 1):
        default = ___delta("tuple-getitem", args, 0)
        try:
            return getattribute(name)
        except AttributeError:
            return default
    else:
        raise TypeError("getattr expected at most 3 arguments")

___assign("%getattr", getattr)

# hasattr(object, name)
# The arguments are an object and a string. The result is True if the string
# is the name of one of the object's attributes, False if not.
def hasattr(obj, name):
    getattr = ___id("%getattr")
    try:
        getattr(obj, name)
    except AttributeError:
        return False
    else:
        return True

___assign("%hasattr", hasattr)
