a_module = __import__("a")
f_class = a_module.F
f_object = f_class('ss')
print(f_object.xxx)
