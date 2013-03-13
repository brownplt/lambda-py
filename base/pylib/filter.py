class filter(object):
    def __init__(self, function, iterable):
        iter = ___id("%iter")
        if function:
            self.function = function
        else:
            self.function = lambda x: x
        self.iterator = iter(iterable)
    def __iter__(self):
        return self
    def __next__(self):
        element = self.iterator.__next__()
        if self.function(element):
            return element
        else:
            return self.__next__()
    def __list__(self):
        result = []
        for element in self:
            result.append(element)
        return result

___assign("%filter", filter)
