import sys

#def strclass(cls): #for debug
#    return "%s.%s" % (cls.__module__, cls.__name__)
def getattr_default(obj, attr):
    try:
        return getattr(obj, attr)
    except AttributeError:
        return None

#------------Result---------------#
class TestResult(object):
    def __init__(self):
        self.failures = []
        self.errors = []
        self.expectedFailures = []
        self.unexpectedSuccess = []
        self.shouldStop = False
        self.testsRun = 0
    def startTest(self, test):
        self.testsRun += 1
    def stopTest(self, test): pass
    def startTestRun(self): pass
    def stopTestRun(self): pass
    def addSuccess(self, test): pass

    def addError(self, test, err):
        self.errors.append((test, self._exc_info_to_string(err, test)))

    def addFailure(self, test, err):
        self.failures.append((test, self._exc_info_to_string(err, test)))

    def addExpectedFailure(self, test, err):
        tmp = (test, self._exc_info_to_string(err, test))
        self.expectedFailures.append(tmp)

    def addUnexpectedSuccess(self, test):
        self.unexpectedSuccesses.append(test)

    def wasSuccessful(self):
        return len(self.failures) == len(self.errors) == 0

    def stop(self):
        self.shouldStop = True

    def _exc_info_to_string(self, err, test):
        return "tmp _exc_info holder"


class TextTestResult(TestResult):
    def __init__(self):
        super().__init__()
        self.showAll = True # temporarily set to True
        self.dots = True #temporarily set to True

    def startTest(self, test):
        super().startTest(test)
        if self.showAll:
            print(test)
    def addSuccess(self, test):
        super().addSuccess(test)
        print("ok")
    def addError(self, test, err):
        super().addError(test, err)
        print("ERROR")
    def addFailure(self, test, err):
        super().addFailure(test, err)
        print("FAIL")
    def addExpectedFailure(self, test, err):
        super().addExpectedFailure(test, err)
        print("expected failure")
    def addUnexpectedSuccess(self, test):
        super().addUnexpectedSuccess(test)
        print("unexpected success")



#---------TestSuite-----------#
class BaseTestSuite(object):
    def __init__(self, *args):
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
            raise TypeError(str(test) + ' is not callable')
        if isinstance(test, type) and issubclass(test,
                                                 (TestCase, TestSuite)):
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
        result = self._makeResult()

        try:
            test(result) #invoke BaseTestSuite.__call__ then
        finally:
            print("Ran " + str(result.testsRun))
        expectedFails = unexpectedSuccesses = 0
        try:
            results = map(len, (result.expectedFailures,
                                result.unexpectedSuccesses))
        except AttributeError:
            pass
        else:
            expectedFails, unexpectedSuccesses = results

        infos = []
        if not result.wasSuccessful():
            print("FAILED")
            failed, errored = len(result.failures), len(result.errors)
            if failed:
                infos.append("failures="+str(failed))
            if errored:
                infos.append("errors=" + str(errored))
        else:
            print("OK")

        if expectedFails:
            infos.append("expected failures=" + str(expectedFails))
        if unexpectedSuccesses:
            infos.append("unexpected successes=" + str(unexpectedSuccesses))

        if infos:
            for info in infos:
                print(info)
        return result

#---------Loader---------#
class TestLoader(object):
    def __init__(self):
        self.prefix = 'test'
        self.suiteClass = TestSuite
    def loadTestsFromModule(self, module):
        tests = []
        #NOTE: get TestCase obj from module. Is dir implemented?
        for name in dir(module):
            obj = getattr_default(module, name)
            if isinstance(obj, type) and issubclass(obj, TestCase):
                tests.append(self.loadTestsFromTestCase(obj))
        tests = self.suiteClass(tests) #return TestSuite
        return tests

    def loadTestsFromTestCase(self, testCaseClass):
        if issubclass(testCaseClass, TestSuite):
            raise TypeError("Test cases should not be derived from TestSuite." \
                                " Maybe you meant to derive from TestCase?")

        testCaseNames = self.getTestCaseNames(testCaseClass)
        #NOTE: has hasattr been implemented?
        if not testCaseNames and hasattr(testCaseClass, 'runTest'):
            testCaseNames = ['runTest']
        #each method will be used to construct an instance of a TestCase's subclass
        #all the subclasses will be wrapped in TestSuite
        loaded_suite = self.suiteClass(map(testCaseClass, testCaseNames))
        return loaded_suite

    def getTestCaseNames(self, testCaseClass):
        #NOTE: getattr
        def isTestMethod(attrname):
            return attrname.startswith(self.prefix) and \
              callable(getattr_default(testCaseClass, attrname))
        #NOTE: has dir been implemented?
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

class _ExpectedFailure(Exception):
    def __init__(self, exc_info):
        super(_ExpectedFailure, self).__init__()
        self.exc_info = exc_info

class _UnexpectedSuccess(Exception):

class TestCase(object):
    failureException = AssertionError

    def __init__(self, *args):
        if len(args) == 0:
            methodName = 'runTest'
        else:
            methodName = args[0]
        self._testMethodName = methodName
        testMethod = getattr_default(self, methodName)
        if testMethod is None and methodName != 'runTest':
            raise ValueError("no such test method in" + self.__class__ + ":" + methodName)
#    def __str__(self): #for debug
#        return "%s (%s)" % (self._testMethodName, strclass(self.__class__))
    def __call__(self, *args):
        # important!
        self.run(*args)
    def setUp(self): pass
    def tearDown(self): pass
    def run(self, *args):
        if len(args) == 0:
            result = TestResult()
        else:
            result = args[0]

        result.startTest(self)

        testMethod = getattr_default(self, self._testMethodName)

        try:
            outcome = _Outcome()
            self._executeTestPart(self.setUp, outcome)
            if outcome.success:
                self._executeTestPart(testMethod, outcome, True) #TEST specific method!
                self._executeTestPart(self.tearDown, outcome)
            if outcome.success:
                result.addSuccess(self)
            else:
                #NOTE: do we need outcome.skipped?
                for exc_info in outcome.errors:
                    result.addError(self, exc_info)
                for exc_info in outcome.failures:
                    result.addFailure(self, exc_info)
                if outcome.unexpectedSuccess is not None:
                    addUnexpectedSuccess = getattr_default(result, 'addUnexpectedSuccess')
                    if addUnexpectedSuccess is not None:
                        addUnexpectedSuccess(self)
                if outcome.expectedFailure is not None:
                    addExpectedFailure = getattr_default(result, 'addExpectedFailure')
                    if addExpectedFailure is not None:
                        addExpectedFailure(self, outcome.expectedFailure)
        finally:
            #result.stopTest(self)
            pass

    def _executeTestPart(self, function, outcome, *args):
        if len(args) == 1:
            isTest = args[0]
        else:
            isTest = False
        try:
            function()
        except _UnexpectedSuccess:
            exc_info = sys.exc_info()
            outcome.success = False
            if isTest:
                outcome.unexpectedSuccess = exc_info
            else:
                outcome.errors.append(exc_info)
        except _ExpectedFailure:
            outcome.success = False
            exc_info = sys.exc_info()
            if isTest:
                outcome.expectedFailure = exc_info
            else:
                outcome.errors.append(exc_info)
        except self.failureException:
            outcome.success = False
            outcome.failures.append(sys.exc_info())
            exc_info = sys.exc_info()
        except:
            outcome.success = False
            outcome.errors.append(sys.exc_info())
    # start assert Statement
    def assertTrue(self, expr, *args):
        if len(args) == 0:
            msg = "assertTrue error"
        else:
            msg = args[0]
        if not expr:
            raise self.failureException(msg)
    def assertFalse(self, expr, *args):
        if len(args) == 0:
            msg = "assertFalse error"
        else:
            msg = args[0]
        if expr:
            raise self.failureException(msg)

    def assertRaises(self, e, f, *args):
        try:
            f(*args)
        except e:
            return
        else:
            raise self.failureException("assertRaises error")

    def assertEqual(self, first, second, *args):
        if len(args) == 0:
            msg = "assertEqual Error"
        else:
            msg = args[0]
        if not first == second:
            raise self.failureException(msg)

    def assertNotEqual(self, first, second, *args):
        if len(args) == 0:
            msg = "assertEqual Error"
        else:
            msg = args[0]
        if first == second:
            raise self.failureException(msg)

    def assertIn(self, member, container, *args):
        if len(args) == 0:
            msg = "assertIn Error"
        else:
            msg = args[0]
        if member not in container:
            self.failureException(msg)

    def assertNotIn(self, member, container, *args):
        if len(args) == 0:
            msg = "assertIn Error"
        else:
            msg = args[0]
        if member not in container:
            self.failureException(msg)

    def assertIs(self, expr1, expr2, *args):
        if len(args) == 0:
            msg = "assertIs Error"
        else:
            msg = args[0]
        if expr1 is not expr2:
            self.failureException(msg)

    def assertIsNot(self, expr1, expr2, *args):
        if len(args) == 0:
            msg = "assertIsNot Error"
        else:
            msg = args[0]
        if expr1 is expr2:
            self.failureException(msg)


#------------main----------------
class TestProgram(object):
    def __init__(self):
        self.module = __main__
        self.testLoader = TestLoader()
        self.testRunner = TextTestRunner
        self.test = self.testLoader.loadTestsFromModule(self.module)
        self.runTests()

    def runTests(self):
        testRunner = self.testRunner()
        self.result = testRunner.run(self.test)

main = TestProgram
