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

# setattr(object, name, value)
# The arguments are an object, a string and an arbitrary value.
# The string may name an existing attribute or a new attribute.
# The function assigns the value to the attribute, provided the object allows it.
def setattr(obj, name, value):
    obj_cls = ___delta("$class", obj)
    obj_cls.__setattr__(obj, name, value)

___assign("%setattr", setattr)

# dir([object])
# Without arguments, return the list of names in the current local scope.
# With an argument, attempt to return a list of valid attributes for that object.
# If the object has a method named __dir__(), this method will be called and must
# return the list of attributes. This allows objects that implement a custom
# __getattr__() or __getattribute__() function to customize the way dir() reports
# their attributes.
# If the object does not provide __dir__(), the function tries its best to gather
# information from the object’s __dict__ attribute, if defined, and from its type object,
# this is implemented in object.__dir__ for instances and type.__dir__ for classes.
def dir(*args):
    if args.__len__() == 0:
        locals = ___id("%locals")
        return locals()
    elif args.__len__() == 1:
        obj = ___delta("tuple-getitem", args, 0)
        obj_cls = ___delta("$class", obj)
        return obj_cls.__dir__(obj)
    else:
        raise TypeError("dir expected at most 1 argument")

___assign("%dir", dir)
