# Single argument with default value
def f1(a=11):
  return a

___assertEqual(f1(), 11)
___assertEqual(f1(22), 22)

# Two arguments with default values
def f2(a, b=2, c=3):
  return (a, b, c)
  
___assertEqual(f2(11), (11, 2, 3))
___assertEqual(f2(11, 22), (11, 22, 3))
___assertEqual(f2(11, 22, 33), (11, 22, 33))
___assertRaises(TypeError, f2)
___assertRaises(TypeError, f2, 11, 22, 33, 44)

# Three arguments with default values
def f3(a=1, b=2, c=3):
  return (a, b, c)
  
___assertEqual(f3(), (1, 2, 3))
___assertEqual(f3(11), (11, 2, 3))
___assertEqual(f3(11, 22), (11, 22, 3))
___assertEqual(f3(11, 22, 33), (11, 22, 33))
___assertRaises(TypeError, f3, 11, 22, 33, 44)

# Default arguments plus vararg
def f4(a, b=2, *c):
  return (a, b, c)

___assertEqual(f4(11), (11, 2, ()))
___assertEqual(f4(11, 22), (11, 22, ()))
___assertEqual(f4(11, 22, 33), (11, 22, (33,)))
___assertEqual(f4(11, 22, 33, 44), (11, 22, (33, 44)))
___assertRaises(TypeError, f4)
