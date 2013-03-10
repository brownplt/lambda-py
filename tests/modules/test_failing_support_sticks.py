# If an error occurs during importing the module,
# the module will not be added to sys.modules
import sys
import support

source = support.TESTFN + ".py"

# generate new file
try:
    f = open(source, "w")
    print("a = 1 / 0", file=f)
except:
    ___assertTrue(False)

# import the new file, which contains a error
___assertRaises(ZeroDivisionError, __import__, support.TESTFN);
# the sys.modules shouldn't contain the TESTFN
___assertNotIn(support.TESTFN, sys.modules)
