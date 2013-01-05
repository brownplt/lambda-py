class Generator:
    def __init__(self, f):
        self.f = f
        return self

    def __iter__(self):
        return self

    def __next__(self):
        return self.f()

	#def send(self, value):
	#def throw()	