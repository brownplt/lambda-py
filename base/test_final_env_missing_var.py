a_module = __import__("a")
F_class = a_module.F
f_object = F_class()
print(f_object.__dict__)
