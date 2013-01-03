
class Foo:
    def getx(self):
        return 1

foo = Foo()

# Unbound methods can be called with object as argument.
___assertEqual(Foo.getx(foo), 1)

# calling a bound method
___assertEqual(foo.getx(), 1)

# The we access a class method from an object, it should become a bound method
getx = foo.getx
___assertEqual(getx(), 1)

# When we set a function as an attribute of an object, it should not treated as method
foo.f = lambda: 42
___assertEqual(foo.f(), 42)

# If we don't pass any argument, it should raise TypeError
___assertRaises(TypeError, lambda: Foo.getx())

