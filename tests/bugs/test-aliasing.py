# aliasing for built-in object
x = []
y = x
x.append(1)
___assertEqual(y, [1])

# aliasing for user-defined object
class A:
  pass

a = A()
b = a
a.x = 1
___assertEqual(b.x, 1)

# aliasing for class object
B = A
A.x = 2
___assertEqual(B.x, 2)
