hit_except = False
hit_else = False
hit_finally = False

try:
    raise Exception('nyaa!')
except:
    hit_except = True
else:
    hit_else = True
finally:
    hit_finally = True

___assertTrue(hit_except)
___assertTrue(hit_finally)
___assertFalse(hit_else)
