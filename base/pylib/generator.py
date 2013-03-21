class generator(object):
    def __init__(self, init, next):
        init(self)
        self.__next__ = next

___assign("%generator", generator)
