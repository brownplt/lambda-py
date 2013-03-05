a = __import__('a')

f = a.F()
print(f.xxx)
print(a.gets())

t = a.gets
print(t())
