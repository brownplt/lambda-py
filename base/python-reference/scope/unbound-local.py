def errorInOuter():
    print(y)
    def inner():
        return y
    y = 1

def errorInInner():
    def inner():
        return y
    inner()
    y = 1

___assertRaises(UnboundLocalError, errorInOuter)
___assertRaises(NameError, errorInInner)
