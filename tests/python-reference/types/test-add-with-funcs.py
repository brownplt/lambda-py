
# The first operand of a binary operation are getting evaluated twice. 

x = 0
def f():    
    global x
    x += 1
    return x

___assertEqual(f(), 1)

x = 0
___assertEqual(f() + 0, 1)

x = 0
___assertEqual(f() + 0 + 0, 1)
