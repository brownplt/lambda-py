# Collect forms thought to be handled correctly by native lexer/parser
# Especially rare forms not handled elsewhere or not yet interpreted
# Useful with --test-parser

def f():
    # Expressions

    1
    3.0e-12
    "String"
    'String'
    """String"""
    '''String'''
    "''''"
    '""""'
    ""
    ''
    """"""
    ''''''
    "a" 'b' """c""" '''d'''
    """\
"""
    "\
    "
    "\\\'\""
    "\a\b\f\n\r\t\v"

    "\000\777\00\77\0\7"
    "\778\788\888"
    "\x00\xff"
    
    b'hello' b"world"
    'hello' "world"
    b'\n' br'\n'

    "\u0000\u1000"
    "\U00000000"
    "\U00001000"
    'ಠ_ಠ'


    '''a''' '''b'''
    '''a\''' b'''


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

    def g():
        yield 1,2
        a = yield b,
        yield

    d > e <= f is not g != h in i

    j + k - l / m * n % o // p | q ^ r & s << t >> u

    v and w or x

    y()(z)(a,b)(c,)[d][::][e::][::f][g::h][i:j:][k:l:m][n:][o:][p:q][:][:,r][s,t]

    del u[:,v]
    del w[x,y]

    a(b=c)(d,e=f)(*g)(**h)(i,j=k,*l,**m)

    f(x for y in z)

    (x for y in z for a in b if c)
    [x for y in z for a in b if c]

    (x for y in z if a if b)
    [x for y in z if a for y in z if a]
    {x:y for z in w}
    {x for y in z}

    not n
    
    +-o

    (1,2)
    (1,2,)
    ()

    {a:1,b:2}
    {b:3,c:4,}
    {a:1}
    {}

    {x,y,z}


    # Statements

    1
    1;
    1;2
    1;2;

    # Indent problems in editor...
    while p: break
    
    while q: continue

    r = s

    t = u = v

    (*w,) = y,*z,a = b

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
    while f: raise g from h

    while h: pass

    assert i
    assert i,i

    global gj
    global gk,gl,gm

    la = 1
    lb = 1
    lc = 1
    ld = 1
    
    def h():
        nonlocal la
        nonlocal lb,lc,ld

    with a, b, c: pass
    with d as e, f as g, h: pass

    if a: 1

    if a: 1
    else: 2

    if a: 1
    elif b: 2

    if a: 1
    elif b: 2
    elif c: 3
    else: 4

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
    def f(*,g=h): pass
    def m(h,i=j,*,k=l,m,**n):pass

    @k
    def l(): pass
    @m()
    @n(o)
    @p.q(r,s=t,*u,**v)
    def w(x,y=z,*a,**b): pass

    class q: pass
    class r(s,t): pass
    class u(v,w,): pass
    class x(y,z=a,*b,**c): pass
    class d(): pass

    @k
    class l(): pass
    @m()
    @n(o)
    @p.q(r,s=t,*u,**v)
    class w(x,y=z,*a,**b): pass
    import a,b as c,d.e as f
    from c.d import (e,f)
    from ... import g as h, i

from sys import *
