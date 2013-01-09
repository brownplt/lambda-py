class F:
    def __init__(self, name):
        self.name = name

m = F("a")
m.__dict__ = {'x':3}
print(m.name)
print(m.__dict__)
