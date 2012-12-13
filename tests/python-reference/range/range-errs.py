___assertRaises(TypeError, range)
___assertRaises(TypeError, range, 1, 2, 3, 4)
___assertRaises(ValueError, range, 1, 2, 0)

___assertRaises(TypeError, range, 0.0, 2, 1)
___assertRaises(TypeError, range, 1, 2.0, 1)
___assertRaises(TypeError, range, 1, 2, 1.0)
___assertRaises(TypeError, range, 1e100, 1e101, 1e101)

___assertRaises(TypeError, range, 0, "spam")
___assertRaises(TypeError, range, 0, 42, "spam")
