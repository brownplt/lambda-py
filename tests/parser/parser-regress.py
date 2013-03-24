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

    (x for y in z)

    1,2

    a if b else c

    lambda a: 0
    lambda *a: 0
    lambda: 0

    yield 1,2
    a = yield b,
    yield

    d > e <= f is not g != h in i

    j + k - l / m * n % o // p | q ^ r & s << t >> u

    v and w or x

    y()(z)(a,b)(c,)[d][::][e::][::f][g::h][i:j:][k:l:m]

    a(b=c)(d,e=f)(*g)(**h)(i,j=k,*l,**m)

    f(x for y in z)

    (x for y in z for a in b if c)
    [x for y in z for a in b if c]

    (x for y in z if a if b)
    [x for y in z if a for y in z if a]

    not n
    
    -o

    (1,2)
    (1,2,)
    ()

    {a:1,b:2}
    {b:3,c:4,}
    {a:1}
    {}


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

    del a
    del a,b,
    del a[b][c]

    while f: raise g
    while f: raise

    while h: pass

    assert i

    global j
    global k,l,m
    nonlocal n
    nonlocal o,p,q

    with a, b, c: pass
    with d as e, f as g, h: pass

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
    def t(u,v=w): pass
    def t(x,y=z,): pass
    def a(**b): pass
    def c(d,**e): pass
    def f(h,i=j,*k,**l): pass

    class q: pass
    class r(s,t): pass
    class u(v,w,): pass

    import x
    import y as z
    from a import *
