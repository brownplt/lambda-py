hit_else = False
hit_finally = False
hit_except = False
hit_inner_except = False
hit_inner_else = False

try:
    try:
        pass
    except:
        hit_inner_except = True
    else:
        hit_inner_else = True

    raise Exception('outer exception')
except:
    hit_except = True
else:
    hit_else = True
finally:
    hit_finally = True

___assertFalse(hit_inner_except)
___assertTrue(hit_inner_else)
___assertFalse(hit_else)
___assertTrue(hit_finally)
___assertTrue(hit_except)
