def __import__(name):
    if name in sys.modules:
        return sys.modules[name]
    else:
        path = name + '.py'
        code = compile(open(path, "r").read(), path, "exec")
        tmp_module = make_module(code)
        sys.modules[name] = tmp_module
        return tmp_module
