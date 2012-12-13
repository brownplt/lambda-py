hit_finally = False
hit_inner_except = False
hit_inner_finally = False

try:
  try:
      raise Exception('inner exception')
  except:
      hit_inner_except = True
  finally:
      hit_inner_finally = True
finally:
  hit_finally = True

___assertTrue(hit_inner_except)
___assertTrue(hit_inner_finally)
___assertTrue(hit_finally)
