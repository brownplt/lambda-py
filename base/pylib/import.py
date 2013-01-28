def __import__(name):
    path = name + ".py"
    code = compile(open(path, "r").read(), path, "exec")
    glb = {}
    exec(code, glb, glb)
    return __module(g)
