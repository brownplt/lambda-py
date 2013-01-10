# the variable in f() is lifted as a local and assigned 
# unbound, because it is the left side of an assignment. 

global_x = 1

def f():
  global_x += 1

___assertRaises(UnboundLocalError, f)
