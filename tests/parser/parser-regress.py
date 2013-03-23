# Collect forms thought to be handled correctly by native lexer/parser
# Especially rare forms not handled elsewhere or not yet interpreted
# Useful with --test-parser

def f():
    # Expressions
    
    1
    3.0e-12
    "String"
    (2)
    {}
    []
    [3]
    [4,5,6]
    [7,8,9,]
    [x for y in z]
    [u for v in w if c]

    1,2

    a if b else c

    lambda a: 0
    lambda *a: 0

    d > e <= f is not g != h in i

    j + k - l / m * n % o // p | q ^ r & s << t >> u

    v and w or x

    y()(z)(a,b)(c,)[d][::][e::][::f][g::h][i:j:][k:l:m]

    not n
    
    -o
    

    # Statements

    # Indent problems in editor...

    while p: break
    
    while q: continue

    r = s

    t += u

    v # expr...

    while w: return
    while x: return y
    while z: return a,b
    while c: return d,e,

    while f: raise g

    while h: pass

    assert i

    global j

    if k: pass

    if l: pass
    else: pass

    try: pass
    finally: pass

    try: pass
    except p: pass
    except q: pass
    except: pass
    else: pass
    finally: pass

    try: pass
    except: pass
    finally: pass

    try: pass
    except t: pass
    except u: pass
    else: pass

    while v: pass

    while w: pass
    else: pass

    for a in b: pass
    for c in d: pass
    else: pass
    for e,f in g,h: pass

    def i(): pass
    def j(k,l,m): pass
    def j(n,o,p,): pass
    def q(r,*s): pass

    class q: pass
    class r(s,t): pass
    class u(v,w,): pass
