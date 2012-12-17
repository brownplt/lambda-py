# If an error occurs during importing the module, 
# the module will not be added to sys.modules
import sys
from support import *

source = TESTFN + ".py"

# generate new file
with open(source, "w") as f:
    print("a = 1 / 0", file=f)

# import the new file, which contains a error
___assertRaises(ZeroDivisionError, __import__, TESTFN);
# the sys.modules shouldn't contain the TESTFN
___assertNotIn(TESTFN, sys.modules)
