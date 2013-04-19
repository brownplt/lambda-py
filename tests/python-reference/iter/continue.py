l = []
n = 0
while n < 10:
    n += 1
    if n % 2 == 0:
        continue
    l.append(n)

___assertEqual(l, [1,3,5,7,9])
