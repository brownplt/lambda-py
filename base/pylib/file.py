class file:
    def __init__(self, path, mode):
        if ___delta("str=", mode, "r"):
            if ___delta("existing-file?", path):
                self = ___delta("file-open", path, mode)
            else:
                raise IOError("No such file: " + path)
        else:
            self = ___delta("file-open", path, mode)
        self.path = path
        self.mode = mode

    def read(self, *args):
        if ___delta("num=", args.__len__(), 0):
            return ___delta("file-readall", self)
        elif ___delta("num=", args.__len__(), 1):
            size = ___delta("tuple-getitem", args, 0)
            return ___delta("file-read", self, size)

    def readline(self):
        return ___delta("file-readline", self)

    def write(self, data):
        return ___delta("file-write", self, data)

    def close(self):
        return ___delta("file-close", self)

    def __str__(self):
        return "<file '" + self.path + "', mode '" + self.mode + "'>"

___assign('%file', file)
___assign('open', file)
# TODO(Sumner): Is %open needed?
#___assign('%open', file)
