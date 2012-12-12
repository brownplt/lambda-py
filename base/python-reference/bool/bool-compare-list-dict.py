x = [1]
___assertIs(x is x, True)
___assertIs(x is not x, False)

___assertIs(1 in x, True)
___assertIs(0 in x, False)
___assertIs(1 not in x, False)
___assertIs(0 not in x, True)

x = {1: 2}
___assertIs(x is x, True)
___assertIs(x is not x, False)

___assertIs(1 in x, True)
___assertIs(0 in x, False)
___assertIs(1 not in x, False)
___assertIs(0 not in x, True)

___assertIs(not True, False)
___assertIs(not False, True)
