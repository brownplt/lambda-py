# When import a module
# the module will be as to sys.modules
import sys
import support

___assertIn("support", sys.modules)
