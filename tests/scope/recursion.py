# recursive functions must work. They aren't that hard, 
# although we should be sure to test...

def f(x):
    def fact(n):
        if n == 0:
            return 1
        else:
            return n * fact(n - 1)
    if x >= 0:
        return fact(x)
    else:
        raise ValueError("x must be >= 0")

___assertEqual(f(6), 720)
