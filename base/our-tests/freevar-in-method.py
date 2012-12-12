def test():
    method_and_var = "var"
    class Test:
        def method_and_var(self):
            return "method"
        def test(self):
            print('test')
            return method_and_var
        def actual_global(self):
            return str("global")
        def str(self):
            return str(self)
    return Test()

t = test()
#___assertEqual(t.test(), "var")
#___assertEqual(t.method_and_var(), "method")
#___assertEqual(t.actual_global(), "global")
print(t.test())
print(t.method_and_var())
print(t.actual_global())

method_and_var = "var"
class Test:
    # this class is not nested, so the rules are different
    def method_and_var(self):
        return "method"
    def test(self):
        return method_and_var
    def actual_global(self):
        return str("global")
    def str(self):
        return str(self)

t = Test()
#___assertEqual(t.test(), "var")
#___assertEqual(t.method_and_var(), "method")
#___assertEqual(t.actual_global(), "global")
print(t.test())
print(t.method_and_var())
print(t.actual_global())
