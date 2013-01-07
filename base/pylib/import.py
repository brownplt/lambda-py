class Module:
    def __init__(self, name):
        self.__name__ = name
        self.__file__ = None
        self.__path__ = None
        self.__code__ = None # ignore this now

def __import__(name):
    m = Module(name)
    f = open(name)
    code = "".join(f.readlines())
    exec(code, m.__dict__)
    return m
