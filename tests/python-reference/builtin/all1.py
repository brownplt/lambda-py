___assertEqual(all([2, 4, 6]), True)
___assertEqual(all([2, None, 6]), False)
___assertRaises(TypeError, all, 10)               # Non-iterable
___assertRaises(TypeError, all)                   # No args
___assertRaises(TypeError, all, [2, 4, 6], [])    # Too many args
___assertEqual(all([]), True)                     # Empty iterator
