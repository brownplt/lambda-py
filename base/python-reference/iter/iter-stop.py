# test_sinkstate_list
# This used to fail
a = list(range(5))
b = iter(a)
___assertEqual(list(b), list(range(5)))
a.extend(range(5, 10))
___assertEqual(list(b), [])

# test_sinkstate_tuple
a = (0, 1, 2, 3, 4)
b = iter(a)
___assertEqual(list(b), list(range(5)))
___assertEqual(list(b), [])

# test_sinkstate_string
a = "abcde"
b = iter(a)
___assertEqual(list(b), ['a', 'b', 'c', 'd', 'e'])
___assertEqual(list(b), [])

#test_sinkstate_callable
#This used to fail
def spam(state=[0]):
    i = state[0]
    state[0] = i+1
    if i == 10:
        raise AssertionError("shouldn't have gotten this far")
    return i
b = iter(spam, 5)

___assertEqual(list(b), list(range(5)))
___assertEqual(list(b), [])

# test_sinkstate_dict
# XXX For a more thorough test, see towards the end of:
# http://mail.python.org/pipermail/python-dev/2002-July/026512.html
a = {1:1, 2:2, 0:0, 4:4, 3:3}
for b in iter(a), a.keys(), a.items(), a.values():
    b = iter(a)
    ___assertEqual(len(list(b)), 5)
    ___assertEqual(list(b), [])

# test_sinkstate_range
a = range(5)
b = iter(a)
___assertEqual(list(b), list(range(5)))
___assertEqual(list(b), [])
