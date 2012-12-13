f1 = lambda x: lambda y: x + y
inc = f1(1)
plus10 = f1(10)
___assertEqual(inc(1), 2)
___assertEqual(plus10(5), 15)
