hit_except = False
hit_else = False

try:
    raise Exception('foo!')
except:
    hit_except = True
else:
    hit_else = True

___assertFalse(hit_else)
___assertTrue(hit_except)
