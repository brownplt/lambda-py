# test import scope
# the import scope is limited to the local
# scope of the import statement
def f():
    import support

f()
try:
    print(support.TESTFN)
    ___fail("import scope broken")
except NameError:
    pass
