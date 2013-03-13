str = "builtin ids can be overwritten without affecting internal operation"
list = []
for iter in str:
    list.append(iter)
___assertEqual(list, [iter for iter in str])
