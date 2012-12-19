# Test the sys.meta_path protocol
 
import sys

class FakeFinder:
    def find_module(self, fullname, path=None):
        if fullname is None:
            return
        if fullname.startswith("fake_"):
            return FakeLoader()

        # If find_module() raises an exception, it will be propagated to the caller, aborting the import.
        if fullname.startswith("fakeerror_"):
            return ErrorLoader()

class FakeLoader:
    def load_module(self, fullname):
        return fullname

class ErrorLoader:
    def load_module(self, fullname):
        1/0

sys.meta_path.append(FakeFinder())

import fake_foo
___assertEqual(fake_foo, "fake_foo")

try:
    import fakeerror_foo
except ZeroDivisionError:
    # as expected
    pass
else:
    ___assertTrue(False)

try:
    import something_else
except ImportError:
    # as expected
    pass
else:
    ___assertTrue(False)
