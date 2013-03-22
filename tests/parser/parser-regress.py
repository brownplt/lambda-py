# Collect forms thought to be handled correctly by native lexer/parser
# Especially rare forms not handled elsewhere or not yet interpreted
# Useful with --test-parser

def f():
    for a in b: pass
    for c in d: pass
    else: pass
    for a,b in c,d: pass
