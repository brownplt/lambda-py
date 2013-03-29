class A:
    def f(self):
        return 'A'
        
# super() in a nested function without arguments
class EE(A):
    def f(self):
        def nested():
            return super().f() + 'E'
        return nested()

# SystemError("super(): no arguments")
___assertRaises(SystemError, EE().f)
