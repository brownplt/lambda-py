try:
    a = __import__('a')
    print(a.s)
except:
    print("exception!") # which now fails to print

