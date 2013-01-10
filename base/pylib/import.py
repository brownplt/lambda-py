def __import__(name):
    path = name + ".py"
    code = compile(open(path, "r").read(), path, "exec")
    g = simpledict()
    for name in code.get_names():
        g = g.bind(name)
    exec(code, g, g)
    return __module(g)
