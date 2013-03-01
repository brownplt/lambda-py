l = []
l.extend([])
___assertEqual(l, [])
l.extend([4])
___assertEqual(l, [4])
l.extend([5, 6])
___assertEqual(l, [4,5,6])
