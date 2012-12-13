def f(): pass
class C: pass
x = C()
if not f: raise Exception('f is false instead of true')
if not C: raise Exception('C is false instead of true')
if not x: raise Exception('x is false instead of true')
