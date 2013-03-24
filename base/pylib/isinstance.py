# isinstance(obj, classinfo): return true if the obj argument is an instance
# of the classinfo argument, or of a subclass thereof.
# classinfo may be a type object, a tuple of type objects, or recusively
# contain other such tuples, otherwise a TypeError exception is raised.
def isinstance(obj, classinfo):
    type = ___id("%type")
    tuple = ___id("%tuple")
    if ___delta("isinstance", classinfo, type):
        return ___delta("isinstance", obj, classinfo)
    elif ___delta("isinstance", classinfo, tuple):
        any = ___id("%any")
        return any((isinstance(obj, cls) for cls in classinfo))
    else:
        raise TypeError("isinstance() arg 2 must be a type or a tuple of types")

___assign("%isinstance", isinstance)

# issubclass(cls, classinfo): return true if cls is a subclass of classinfo.
# A class is considered a subclass of itself.
# classinfo may be a tuple of class objects, in which case every entry in classinfo
# will be checked. In any other case, a TypeError exception is raised.
def issubclass(cls, classinfo):
    type = ___id("%type")
    tuple = ___id("%tuple")
    if not type in ___delta("$class", cls).__mro__:
        raise TypeError("issubclass() arg 1 must be a class")
    if type in ___delta("$class", classinfo).__mro__:
        return classinfo in cls.__mro__
    elif tuple in ___delta("$class", classinfo).__mro__:
        any = ___id("%any")
        return any((issubclass(cls, c) for c in classinfo))
    else:
        raise TypeError("issubclass() arg 2 must be a class or a tuple of classes")

___assign("%issubclass", issubclass)
