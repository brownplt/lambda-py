# isinstance(obj, classinfo): return true if the obj argument is an instance
# of the classinfo argument, or of a subclass thereof.
# classinfo may be a type object, a tuple of type objects, or recusively
# contain other such tuples, otherwise a TypeError exception is raised.
# TypeError exceptions are raised by the "isinstance" primitive operation.
def isinstance(obj, classinfo):
    if ___delta("isinstance", classinfo, tuple):
        return any((isinstance(obj, cls) for cls in classinfo))
    else:
        return ___delta("isinstance", obj, classinfo)
        
___assign("%isinstance", isinstance)
