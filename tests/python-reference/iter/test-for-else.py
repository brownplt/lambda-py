def index(iterable, item):
    i = 0
    for it in iterable:
        if it == item:
            break
        i += 1
    else:
        i = None
    return i

___assertEqual(index('abcde', 'c'), 2)
___assertEqual(index('abcde', 'C'), None)
