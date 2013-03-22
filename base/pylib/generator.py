class generator(object):
    def __init__(self, init):
        init(self)

    # TODO(dbp): handle return arguments
    def __next__(self):
        # print("resume  is:")
        # print(self.___resume)
        # print(".")
        return self.___resume(None)

    def __iter__(self):
        return self
        
    def __list__(self):
        return [x for x in self]

___assign("%generator", generator)
