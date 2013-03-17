# callable(object)
# Return True if the object argument appears callable, False if not.
# Note that classes are callable (calling a class returns a new instance);
# instances are callable if their class has a __call__() method.
def callable(obj):
    try:
        obj.__call__
    except:
        # obj has no __call__ attribute, use the primary operator
        # until functions become objects with a __call__ attribute.
        return ___delta("is-func?", obj)
    else:
        # No exception, obj has __call__ attribute
        return True

___assign("%callable", callable)   