# calling built-in types without argument must return empty
___assertEqual(tuple(), ())
t0_3 = (0, 1, 2, 3)
t0_3_bis = tuple(t0_3)
___assertTrue(t0_3 is t0_3_bis)
___assertEqual(tuple([]), ())
___assertEqual(tuple([0, 1, 2, 3]), (0, 1, 2, 3))
___assertEqual(tuple(''), ())
___assertEqual(tuple('spam'), ('s', 'p', 'a', 'm'))
