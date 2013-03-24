class generator(object):
    def __init__(self, init):
        init(self)

    def __next__(self):
        return self.___resume(None)

    def send(self, arg):
        return self.___resume(arg)

    def __iter__(self):
        return self
        
    def __list__(self):
        return [x for x in self]

___assign("%generator", generator)
