hit_except = False
hit_finally = False

try:
    raise Exception('yarr!')
except:
    hit_except = True
finally:
    hit_finally = True

___assertTrue(hit_except)
___assertTrue(hit_finally)
