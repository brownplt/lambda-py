class A:
    def f(self):
        return 'A'
        
# super() in a nested function with non-self argument
class EE(A):
    def f(self):
        def nested(self):
            return super().f() + 'E'
        return nested(5)

# TypeError("super(type, obj): obj must be an instance or subtype of type")
___assertRaises(TypeError, EE().f)
