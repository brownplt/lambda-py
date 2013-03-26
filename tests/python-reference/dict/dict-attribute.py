class C:
    def __init__(self):
        self.a = 1

assert "__init__" in C.__dict__, "Class C has no __init__ attribute"

assert "a" in C().__dict__, "instance of class C has no 'a' attribute"

