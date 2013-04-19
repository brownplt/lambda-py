# test filter predicates in list comprehensions
___assertEqual([x for x in [1, 'hello', [], [3], '', None, 9, 0] if x], [1, 'hello', [3], 9])
___assertEqual([x for x in [1, -3, 9, 0, 2] if x > 0], [1, 9, 2])
___assertEqual([c for c in 'Hello World' if 'a' <= c <= 'z'], list('elloorld'))
___assertEqual([c for c in 'Hello World' if 'a' <= c <= 'z' and c != 'l'], list('eoord'))
