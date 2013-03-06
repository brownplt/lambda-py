def __import__(name):
    path = name + '.py'
    code = compile(open(path, "r").read(), path, "exec")
    return make_module(code)
