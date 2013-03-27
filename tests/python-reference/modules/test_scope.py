# test import scope
# the import scope is limited to the local
# scope of the import statement
def f():
    import support
    try:
        tmp = support.TESTFN
    except AttributeError:
        ___fail("import Error")
    else:
        # import works
        return
        
        

f()
try:
    s = support.TESTFN
    ___fail("import scope broken")
except NameError:
    pass
else:
    ___fail("import scope broken")
