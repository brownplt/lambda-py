hit_except = False
hit_else = False

try:
    pass
except:
    hit_except = True
else:
    hit_else = True

___assertFalse(hit_except)
___assertTrue(hit_else)
