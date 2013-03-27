chk = 'init'
while True:
  try:
    chk = 'in loop'
    break
    chk = 'better-not-be-this'
  finally:
    if chk == 'in loop':
      chk = 100

___assertEqual(chk, 100)
