
try: 
 class C(object):
  x = 4
  class D(object):
   y = x
except:
 assert True
else:
 assert False 

