class generator(object):
    def __init__(self, init):
        init(self)

    # TODO(dbp): handle return arguments
    def __next__(self):
        return self.___resume(None)

___assign("%generator", generator)
