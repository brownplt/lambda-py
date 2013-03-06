# isinstance(obj, classinfo): return true if the obj argument is an instance
# of the classinfo argument, or of a subclass thereof.
# classinfo may be a type object, a tuple of type objects, or recusively
# contain other such tuples, otherwise a TypeError exception is raised.
def isinstance(obj, classinfo):
    if type in ___delta("$class", classinfo).__mro__:
        return classinfo in ___delta("$class", obj).__mro__
    elif tuple in ___delta("$class", classinfo).__mro__:
        return any((isinstance(obj, cls) for cls in classinfo))
    else:
        raise TypeError("isinstance() arg 2 must be a type or a tuple of types")

___assign("%isinstance", isinstance)
