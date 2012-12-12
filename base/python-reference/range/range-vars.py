a = 10
b = 100
c = 50

___assertEqual(list(range(a, a+2)), [a, a+1])
___assertEqual(list(range(a+2, a, -1)), [a+2, a+1])
___assertEqual(list(range(a+4, a, -2)), [a+4, a+2])

seq = list(range(a, b, c))
___assertIn(a, seq)
___assertNotIn(b, seq)
___assertEqual(len(seq), 2)

seq = list(range(b, a, -c))
___assertIn(b, seq)
___assertNotIn(a, seq)
___assertEqual(len(seq), 2)

seq = list(range(-a, -b, -c))
___assertIn(-a, seq)
___assertNotIn(-b, seq)
___assertEqual(len(seq), 2)
