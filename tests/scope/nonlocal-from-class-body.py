class C:
    x = 3
    def f(self):
        nonlocal x
        x = 2
        return x

c = C()

