x = 10
l = [x for x in range(5)]
___assertEqual(x, 10)

try:
    l = [x/0 for x in range(5)]
except:
    ___assertEqual(x, 10)
else:
    ___fail()
