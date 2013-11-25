class F:
    def __init__(self):
        self.x={}
    def __setitem__(self, *i):
        pos, v = i
        self.x[pos] = v
    def __getitem__(self, i):
        return self.x[i]
    
def setitem(obj, *i):
    obj.x[i[0]]=333
    
f = F()
f.__setitem__ = setitem # __setitem__ on object

f['t'] = 34
___assertEqual(f['t'], 34)  

F.__setitem__ = setitem # __setitem__ on class
f['z'] = 0
___assertEqual(f['z'], 333)
