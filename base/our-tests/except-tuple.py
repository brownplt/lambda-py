x = (TypeError, AttributeError)
try:
    raise TypeError('test')
except x:
    print('fuckle')
