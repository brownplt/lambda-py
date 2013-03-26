chk = 'init'
while True:
  if chk is 'saw while':
    chk = 100
    break
  try:
    chk = 'in loop'
    continue
    chk = 'better-not-be-this'
  finally:
    if chk is 'in loop':
      chk = 'saw while'

___assertEqual(chk, 100)
