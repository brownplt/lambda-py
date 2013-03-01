def f():
  while True:
    try:
      pass
    finally:
      continue

___assertRaises(SyntaxError, f)
