___assertEqual(list(filter(lambda c: 'a' <= c <= 'z', 'Hello World')), list('elloorld'))
___assertEqual(list(filter(None, [1, 'hello', [], [3], '', None, 9, 0])), [1, 'hello', [3], 9])
___assertEqual(list(filter(lambda x: x > 0, [1, -3, 9, 0, 2])), [1, 9, 2])
def badfunc():
    pass
___assertRaises(TypeError, list, filter(badfunc, range(5)))

# test bltinmodule.c::filtertuple()
___assertEqual(list(filter(None, (1, 2))), [1, 2])
___assertEqual(list(filter(lambda x: x>=3, (1, 2, 3, 4))), [3, 4])
___assertRaises(TypeError, list, filter(42, (1, 2)))
