hit_except = False
hit_finally = False

try:
    pass
except:
    hit_except = True
finally:
    hit_finally = True

___assertFalse(hit_except)
___assertTrue(hit_finally)
