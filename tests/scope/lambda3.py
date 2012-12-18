f3 = lambda x: lambda y: global_x + y
global_x = 1
inc = f3(None)
___assertEqual(inc(2), 3)
