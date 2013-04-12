class A:
    def f(self):
        return 'A'
 
# super() in an external function with "self" argument
def external(self):
    super().f() + 'E'

class EE(A):
    def f(self):
        return external(self)

# SystemError("super(): no arguments")
___assertRaises(SystemError, EE().f)
