# alias shouldn't affect immutable object
x = 1
y = x
x = 2
___assertEqual(y, 1)
___assertEqual(x, 2)

y = 3
___assertEqual(x, 2)
___assertEqual(y, 3)

t = (1,2)
t1 = t
t = (1,2,3)
___assertEqual(t1, (1,2))

t1 = (1,2,3,4)
___assertEqual(t1, (1,2,3,4))
___assertEqual(t, (1,2,3))

s = "1"
s1 = s
s = "2"
___assertEqual(s1, "1")

s1 = "3"
___assertEqual(s1, "3")
___assertEqual(s, "2")
