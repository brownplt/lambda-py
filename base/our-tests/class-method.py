class C:
    def f(self):
        self.g()
        g(self)
    def g(self):
        print("heyyy")

c = C()
c.f()
