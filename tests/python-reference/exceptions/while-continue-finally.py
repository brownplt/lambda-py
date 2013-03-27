chk = 'init'
while True:
  if chk == 'saw while':
    chk = 100
    break
  try:
    chk = 'in loop'
    continue
    chk = 'better-not-be-this'
  finally:
    if chk == 'in loop':
      chk = 'saw while'

___assertEqual(chk, 100)
