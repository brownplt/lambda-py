# def __import__(name):
#     if name in sys.modules:
#         return sys.modules[name]
#     else:
#         f = None
#         for path in sys.path:
#             file_with_path = path + '/' + name + '.py'
#             #print("checking: " + file_with_path)
#             try:
#                 f = open(file_with_path, "r")
#                 break
#             except IOError:
#                 continue
#             #print("f is " + str(f))
#         if f == None:
#             print("f == None?:" + str(f == None))
#             raise ImportError("module " + name + " not found")
#         else:
#             code = compile(f.read(),
#                            file_with_path,
#                            "exec")
#             tmp_module = make_module(code)
#             sys.modules[name] = tmp_module
#             return tmp_module


def __import__(name):
    open = ___id("%open")
    compile = ___id("%compile")
    make_module = ___id("%make_module")

    if name in sys.modules:
        return sys.modules[name]

    for path in sys.path:
        file_with_path = path + '/' + name + '.py'
        if not ___delta("existing-file?", file_with_path):
            continue
        else:
            f = open(file_with_path, "r")
            code = compile(f.read(),
                           file_with_path,
                           "exec")
            tmp_module = make_module(code)
            tmp_module.__name__ = name
            sys.modules[name] = tmp_module
            return tmp_module

    raise ImportError("module " + name + " not found")
