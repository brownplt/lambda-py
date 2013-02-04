class A:
    @staticmethod
    def sm1(who):
        return "Hello " + who

___assertEqual(A.sm1("World"), "Hello World")
___assertEqual(A().sm1("World"), "Hello World")
