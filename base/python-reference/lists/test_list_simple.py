if (not (list([]) == [])): raise Exception('List empty')
l0_3 = [0, 1, 2, 3]
l0_3_bis = list(l0_3)
if (not (l0_3 == l0_3_bis)): raise Exception('List identity')
if (not (l0_3 is not l0_3_bis)): raise Exception('List identity')
if (not (list(()) == [])): raise Exception( 'List tuple')
if (not (list((0, 1, 2, 3)), [0, 1, 2, 3])): raise Exception('List tuple 2')
if (not (list('') == [])): raise Exception( 'List string 1')
if (not (list('spam') == ['s', 'p', 'a', 'm'])): raise Exception( 'List string 2')

