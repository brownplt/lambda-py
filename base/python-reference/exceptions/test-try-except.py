hit_except = False

try:
    raise Exception('ahoy!')
except:
    hit_except = True

___assertTrue(hit_except)
