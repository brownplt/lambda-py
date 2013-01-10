class Foo:
    def __getitem__(*a):
        return 0

foo = Foo()
___assertEqual(foo[2], 0)


# a[i] should use __getitem__ from class, not from object.
# This applies to all __xxx__ methods, not just __getitem__.
def getitem(*a):
    return 42
foo.__getitem__ = getitem
___assertEqual(foo[2], 0)

# The __xxx__ methods are for the objects. They shoundn't be accessible to the class it self.
___assertRaises(TypeError, lambda: Foo[1])
