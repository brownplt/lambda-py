hit_except = False
hit_else = False
hit_finally = False

try:
    pass
except:
    hit_except = True
else:
    hit_else = True
finally:
    hit_finally = True

___assertFalse(hit_except)
___assertTrue(hit_finally)
___assertTrue(hit_else)
