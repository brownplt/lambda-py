# test reading the module correctly
# this test can be used to test .pyc

from support import *
import sys

def test_with_extension(ext):
    source = TESTFN + ext
    # write the statement to the source file
    # for testing the module.
    with open(source, "w") as f:
        print("# This tests Python's ability to import a",
              ext, "file.", file=f)
        a = 10
        b = 15
        print("a =", a, file=f)
        print("b =", b, file=f)

    try:
        mod = __import__(TESTFN)
    except ImportError as err:
        ___fail("import from %s failed: %s" % (ext, err))
    # checking the module
    ___assertEqual(mod.a, a)
    # checking the module
    ___assertEqual(mod.b, b)

test_with_extension(".py")
