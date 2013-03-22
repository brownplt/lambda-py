class generator(object):
    def __init__(self, init):
        init(self)

    # TODO(dbp): handle return arguments
    def __next__(self, *args):
        if len(args) > 0:
            return self.___resume(args[0])
        else:
            return self.___resume(None)

    def __iter__(self):
        return self
        
    def __list__(self):
        return [x for x in self]

___assign("%generator", generator)
