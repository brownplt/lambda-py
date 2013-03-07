# for mutable object
class F: pass
f = F()
d = f # d is an alias
d.x = 1
___assertEqual(f, d)
___assertEqual(f.x, 1)

f.y = 2
___assertEqual(f, d)
___assertEqual(d.y, 2)

# dict
d = {1:2, 3:4}
d1 = d
d1[1] = 10
___assertEqual(d, d1)
___assertEqual(d, {1:10, 3:4})

d[3] = 5
___assertEqual(d, d1)
___assertEqual(d1, {1:10, 3:5})

# list
l = [1,2,3,4]
l1 = l
l[0] = 12
___assertEqual(l, l1)
___assertEqual(l1, [12,2,3,4])

l1[1] = 13
___assertEqual(l, l1)
___assertEqual(l, [12,13,3,4])
