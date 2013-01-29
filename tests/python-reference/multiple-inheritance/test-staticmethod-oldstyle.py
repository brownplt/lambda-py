class A:
    def sm2(who):
        return "Hello " + who
    sm2 = staticmethod(sm2)

___assertEqual(A.sm2("World"), "Hello World")
___assertEqual(A().sm2("World"), "Hello World")
