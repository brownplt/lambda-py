
code = compile("b = a; a = 2", "<stdin>", "exec")

g = simpledict().bind("a").bind("b")
g['a'] = 1
exec(code, g, simpledict())
___assertEqual(g['b'], 1)
___assertEqual(g['a'], 2)


