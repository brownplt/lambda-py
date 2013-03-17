# should be able to import sys module
# and it should be present in sys.modules

import sys
___assertIn("sys", sys.modules)

import sys as sys2
___assertIs(sys, sys2)
___assertIs(sys.path, sys2.path)

from sys import path
from sys import path as path2
___assertIs(path, sys.path)
___assertIs(path2, sys.path)

___assertEqual(sys.__name__, "sys")
___assertEqual(sys2.__name__, "sys")
