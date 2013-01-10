a_module = __import__("a")
f_object = a_module.F()
print(f_object.__dict__)
