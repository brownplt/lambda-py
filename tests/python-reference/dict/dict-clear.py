d = {1:1, 2:2, 3:3}
d.clear()
___assertEqual(d, {})

___assertRaises(TypeError, d.clear, None)
