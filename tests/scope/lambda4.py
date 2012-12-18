f8 = lambda x, y, z: lambda a, b, c: lambda : z * (b + y)
g = f8(1, 2, 3)
h = g(2, 4, 6)
___assertEqual(h(), 18)
