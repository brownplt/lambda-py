def f():
    n = 1000
    #while n < 10:
    yield n
    221 + 10
    yield n
    n = 23
    yield n
    n = 24
    yield n
    n = 25
    yield n
    n = 26
    
g = f()
#for x in f():
#    pass

print(next(g))
print("gap")
print(next(g))
print("gap")
print(next(g))
print("gap")
print(next(g))
print("gap")
print(next(g))
# print(next(g))
# print(next(g))
# print(next(g))
# print(next(g))
# print(next(g))
# print(next(g))
# print(next(g))

#print([x for x in f()])
