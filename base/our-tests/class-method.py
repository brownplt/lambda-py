class C:
    def f(self):
        self.g()
    def g(self):
        print("heyyy")

c = C()
c.f()
