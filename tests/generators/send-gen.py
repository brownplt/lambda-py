def coroutine(seq):
	count = 0
	while count < 200:
		count += yield
		seq.append(count)

seq = []
c = coroutine(seq)

next(c)
___assertEqual(seq, [])

c.send(10)
___assertEqual(seq, [10])

c.send(10)
___assertEqual(seq, [10, 20])