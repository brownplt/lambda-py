def f():
  x = 3
  y = 4
  a = locals() 
  print(a) # {'x': 3, 'y': 4}
  z = 2
  b = locals() 
  print(b) # {'x': 3, 'y': 4, 'z': 2, 'a': a}
  return a, b

a, b = f()
___assertEqual(a == {'x': 3, 'y': 4, 'z': 2, 'a': a})
___assertEqual(b == {'x': 3, 'y': 4, 'z': 2, 'a': a})
___assertIs(a is b)
