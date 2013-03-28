x = 12
y = 13

def f():
    for x in [3,4]:
        for y in [5, 6]:
            pass

assert x == 12
assert y == 13

class c:
    for x in [7,8]:
        for y in [9, 10]:
            pass


assert x == 12
assert y == 13

assert c().x == 8
assert c().y == 10

