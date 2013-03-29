class A:
    def f(self):
        return 'A'
        
# super() in a nested function with "self" argument
class EE(A):
    def f(self):
        def nested(this):
            return super().f() + 'E'
        return nested(self)

___assertEqual(EE().f(), 'AE')
