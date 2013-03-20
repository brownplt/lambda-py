#------------Result---------------#
class TestResult(object):
    def addSuccess(self, test):
        pass

class TextTestResult(TestResult):
    pass

#---------TestSuite-----------#
class BaseTestSuite(object):
    def __init__(self, *args):
        """original one is __init__(self, tests=())"""
        if len(args) == 0:
            tests = ()
        else:
            tests = args[0]
        self._tests = []
        self.addTests(tests)
    def addTests(self, tests):
        if isinstance(tests, str):
            raise TypeError("tests must be an iterable of tests, not a string")
        for test in tests:
            self.addTest(test)
    def addTest(self, test):
        if not callable(test):
            raise TypeError(str(test) + 'is not callable')
        if isinstance(test, type) and issubclass(test,
                                                 (case.TestCase, TestSuite)):
            raise TypeError("TestCases and TestSuites must be instantiated "
                            "before passing them to addTest()")
        self._tests.append(test)
    def __call__(self, *args):
        # important!
        return self.run(*args)

    def run(self, result):
        pass

class TestSuite(BaseTestSuite):
    def run(self, result):
        for test in self._tests:
            test(result)
        return result

#-------------Runner---------------#
class TextTestRunner(object):
    def __init__(self):
        self.resultclass = TextTestResult
    def _makeResult(self):
        return self.resultclass()

    def run(self, test):
        "Run the given test case or test suite."
        result = self._makeResult()
        try:
            test(result) #invoke BaseTestSuite.__call__ then
        finally:
            pass
            #TODO: print result
        return result

#---------Loader---------#
class TestLoader(object):
    testMethodPrefix = 'test'
    suiteClass = TestSuite
    def loadTestFromModules(self, module):
        tests = []
        #NOTE: get TestCase obj from module. Is dir implemented?
        for name in dir(module):
            obj = getattr(module, name)
            if isinstance(obj, type) and issubclass(obj, case.TestCase):
                tests.append(self.loadTestsFromTestCase(obj))
        tests = self.suiteClass(tests) #return TestSuite
        return tests

    def loadTestsFromTestCase(self, testCaseClass):
        "Return a suite of all tests caess contained in testCaseClass"
        testCaseNames = self.getTestCaseNames(testCaseClass)
        #NOTE: is hasattr implemented?
        if not testCaseNames and hasattr(testCaseClass, 'runTest'):
            testCaseNames = ['runTest']
        #each method will be used to construct an instance of a TestCase's subclass
        #all the subclasses will be wrapped in TestSuite
        loaded_suite = self.suiteClass(map(testCaseClass, testCaseNames))
        return loaded_suite

    def getTestCaseNames(self, testCaseClass):
        "Return a sequence of method names found within testCaseClass."
        #NOTE: getattr
        def isTestMethod(attrname):
            return attrname.startswith(prefix) and \
              callable(getattr(testCaseClass, attrname))
        #NOTE: Is dir implemented?
        testFnNames = list(filter(isTestMethod, dir(testCaseClass)))
        return testFnNames

#---------TestCase-------#
class _Outcome(object):
    def __init__(self):
        self.success = True
        self.skipped = None
        self.unexpectedSuccess = None
        self.expectedFailure = None
        self.errors = []
        self.failures = []

class TestCase(object):
    def __init__(self, *args):
        """original one is __init__(self, methodName='runTest')"""
        if len(args) == 0:
            methodName = 'runTest'
        else:
            methodName = args[0]
        self._testMethodName = methodName
        try:
            testMethod = getattr(self, methodName)
        except AttributeError:
            if methodName != 'runTest':
                raise ValueError("no such test method in" + self.__class__ + ":" + methodName)
    def setUp(self): pass
    def tearDown(self): pass
    def run(self, *args):
        """original one is run(self, result=None)"""
        if len(args) == 0:
            result = TestResult()
        else:
            result = args[0]

        testMethod = getattr(self, self._testMethodName)

        try:
            outcome = _Outcome()
            self._executeTestPart(self.setUp, outcome)
            if outcome.success:
                self._executeTestPart(testMethod, outcome) #TEST specific method!
                self._executeTestPart(self.tearDown, outcome)
            if outcome.success:
                result.addSuccess(self)
            else:
                #NOTE: do we need outcome.skipped
                for exc_info in outcome.errors:
                    result.addError(self, exc_info)
                for exc_info in outcome.failures:
                    result.addFailure(self, exc_info)
                if outcome.unexpectedSuccess is not None:
                    addUnexpectedSuccess = getattr(result, 'addUnexpectedSuccess', None)
                    if addUnexpectedSuccess is not None:
                        addUnexpectedSuccess(self)
                if outcome.expectedFailure is not None:
                    addExpectedFailure = getattr(result, 'addExpectedFailure', None)
                    if addExpectedFailure is not None:
                        addExpectedFailure(self, outcome.expectedFailure)
        finally:
            #result.stopTest(self)
            pass


def main():
    module = __import__('__main__')
    test = loader.loadTestFromModules(module)

    runner = TextTestRunner()
    result = runner.run(test)
