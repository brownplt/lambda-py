class file:
    def __new__(self, *args):
      path = args.__getitem__(0)
      mode = args.__getitem__(1)
      if mode == "r":
        if ___delta("existing-file?", path):
          return ___delta("file-open", path, mode)
        else:
          raise IOError("No such file: " + path)
      else:
        return ___delta("file-open", path, mode)

    def __init__(self, path, mode):
        self.path = path
        self.mode = mode
        self.closed = False

    def read(self, *args):
        if self.closed:
            raise ValueError("I/O operation on closed file.")
        ___assign('%str', str)
        if ___delta("num=", args.__len__(), 0):
            return ___delta("file-readall", self, str)
        elif ___delta("num=", args.__len__(), 1):
            size = ___delta("tuple-getitem", args, 0)
            return ___delta("file-read", self, size, str)

    def readline(self):
        if self.closed:
            raise ValueError("I/O operation on closed file.")
        ___assign('%str', str)
        return ___delta("file-readline", self, str)

    def write(self, data):
        if self.closed:
            raise ValueError("I/O operation on closed file.")
        return ___delta("file-write", self, data)

    def close(self):
        if self.closed:
            return
        self.closed = True
        return ___delta("file-close", self)

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    def __str__(self):
        return "<file '" + self.path + "', mode '" + self.mode + "'>"

___assign('%file', file)
___assign('open', file)
___assign('%open', file)
