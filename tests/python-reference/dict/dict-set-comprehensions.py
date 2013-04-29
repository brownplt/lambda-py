sc = {x for x in (1, 2, 2, 3)}
s = {1, 2, 3}
___assertEqual(sc, s)

dc = {x: x + 1 for x in range(3)}
d = {0:1, 1:2, 2:3}
___assertEqual(dc, d)
