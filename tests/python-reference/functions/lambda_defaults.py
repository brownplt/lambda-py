# Single argument with default value
f1 = lambda a=11: a

___assertEqual(f1(), 11)
___assertEqual(f1(22), 22)

# Two arguments with default values
f2 = lambda a, b=2, c=3: (a, b, c)
  
___assertEqual(f2(11), (11, 2, 3))
___assertEqual(f2(11, 22), (11, 22, 3))
___assertEqual(f2(11, 22, 33), (11, 22, 33))
___assertRaises(TypeError, f2)
___assertRaises(TypeError, f2, 11, 22, 33, 44)

# Three arguments with default values
f3 = lambda a=1, b=2, c=3: (a, b, c)
  
___assertEqual(f3(), (1, 2, 3))
___assertEqual(f3(11), (11, 2, 3))
___assertEqual(f3(11, 22), (11, 22, 3))
___assertEqual(f3(11, 22, 33), (11, 22, 33))
___assertRaises(TypeError, f3, 11, 22, 33, 44)

# Default arguments plus vararg
f4 = lambda a, b=2, *c: (a, b, c)

___assertEqual(f4(11), (11, 2, ()))
___assertEqual(f4(11, 22), (11, 22, ()))
___assertEqual(f4(11, 22, 33), (11, 22, (33,)))
___assertEqual(f4(11, 22, 33, 44), (11, 22, (33, 44)))
___assertRaises(TypeError, f4)
