___assertEqual(any([None, None, None]), False)
___assertEqual(any([None, 4, None]), True)
___assertRaises(TypeError, any, 10)               # Non-iterable
___assertRaises(TypeError, any)                   # No args
___assertRaises(TypeError, any, [2, 4, 6], [])    # Too many args
___assertEqual(any([]), False)                    # Empty iterator
