class TestResult(object):
    """Holder for test result information.

    Test results are automatically managed by the TestCase and TestSuite
    classes, and do not need to be explicitly manipulated by writers of tests.

    Each instance holds the total number of tests run, and collections of
    failures and errors that occurred among those test runs. The collections
    contain tuples of (testcase, exceptioninfo), where exceptioninfo is the
    formatted traceback of the error that occurred.
    """
    _previousTestClass = None
    _testRunEntered = False
    _moduleSetUpFailed = False
    def __init__(self, stream=None, descriptions=None, verbosity=None):
        self.failfast = False
        self.failures = []
        self.errors = []
        self.testsRun = 0
        self.skipped = []
        self.expectedFailures = []
        self.unexpectedSuccesses = []
        self.shouldStop = False
        self.buffer = False
        self._stdout_buffer = None
        self._stderr_buffer = None
        self._original_stdout = sys.stdout
        self._original_stderr = sys.stderr
        self._mirrorOutput = False

    def printErrors(self):
        "Called by TestRunner after test run"

    def startTest(self, test):
        "Called when the given test is about to be run"
        self.testsRun += 1
        self._mirrorOutput = False

    def startTestRun(self):
        """Called once before any tests are executed.

        See startTest for a method called before each test.
        """

    def stopTest(self, test):
        """Called when the given test has been run"""
        self._mirrorOutput = False

    def stopTestRun(self):
        """Called once after all tests are executed.

        See stopTest for a method called after each test.
        """

    def addSuccess(self, test):
        "Called when a test has completed successfully"
        pass

    def addSkip(self, test, reason):
        """Called when a test is skipped."""
        self.skipped.append((test, reason))

    def addExpectedFailure(self, test, err):
        """Called when an expected failure/error occured."""
        self.expectedFailures.append(
            (test, self._exc_info_to_string(err, test)))

    def addUnexpectedSuccess(self, test):
        """Called when a test was expected to fail, but succeed."""
        self.unexpectedSuccesses.append(test)

    def wasSuccessful(self):
        "Tells whether or not this result was a success"
        return len(self.failures) == len(self.errors) == 0

    def stop(self):
        "Indicates that the tests should be aborted"
        self.shouldStop = True

    def _exc_info_to_string(self, err, test):
        """Converts a sys.exc_info()-style tuple of values into a
        string."""
        return "not implemented"

    def _is_relevant_tb_level(self, tb):
        return '__unittest' in tb.tb_frame.f_globals

    def _count_relevant_tb_levels(self, tb):
        length = 0
        while tb and not self._is_relevant_tb_level(tb):
            length += 1
            tb = tb.tb_next
        return length

    def __repr__(self):
        return "not implemented __repr__"

class _Outcome(object):
    def __init__(self):
        self.success = True
        self.skipped = None
        self.unexpectedSuccess = None
        self.expectedFailure = None
        self.errors = []
        self.failures = []

class _ExpectedFailure(Exception):
    """
    Raise this when a test is expected to fail.

    This is an implementation detail.
    """

    def __init__(self, exc_info):
        super(_ExpectedFailure, self).__init__()
        self.exc_info = exc_info

class _UnexpectedSuccess(Exception):
    """
    The test was supposed to fail, but it didn't!
    """


class TestCase(object):
    failureException = AssertionError

    def __init__(self):
        """Create an instance of the class that will use the named test
           method when executed. Raises a ValueError if the instance does
           not have a method with the specified name.
        """
        methodname = 'runTest'
        self._testMethodName = methodName
        self._outcomeForDoCleanups = None
        self._testMethodDoc = 'No test'
        try:
            testMethod = self.methodName
        except AttributeError:
            if methodName != 'runTest':
                # we allow instantiation with no explicit method name
                # but not an *incorrect* or missing method name
                raise ValueError("no such test method: " + methodName)
        else:
            self._testMethodDoc = testMethod.__doc__
        self._cleanups = []

        # Map types to custom assertEqual functions that will compare
        # instances of said type in more detail to generate a more useful
        # error message.
        # self._type_equality_funcs = {}
        # self.addTypeEqualityFunc(dict, 'assertDictEqual')
        # self.addTypeEqualityFunc(list, 'assertListEqual')
        # self.addTypeEqualityFunc(tuple, 'assertTupleEqual')
        # self.addTypeEqualityFunc(set, 'assertSetEqual')
        # self.addTypeEqualityFunc(frozenset, 'assertSetEqual')
        # self.addTypeEqualityFunc(str, 'assertMultiLineEqual')

    def setUp(self):
        "Hook method for setting up the test fixture before exercising it."
        pass

    def tearDown(self):
        "Hook method for deconstructing the test fixture after testing it."
        pass

    @classmethod
    def setUpClass(cls):
        "Hook method for setting up class fixture before running tests in the class."

    @classmethod
    def tearDownClass(cls):
        "Hook method for deconstructing the class fixture after running all tests in the class."

    def defaultTestResult(self):
        return TestResult()


    def _executeTestPart(self, function, outcome, isTest):
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

    def run(self, result=None):
        orig_result = result
        if result is None:
            result = self.defaultTestResult()
            try:
                startTestRun = result.startTestRun
            except:
                startTestRun = None
            if startTestRun is not None:
                startTestRun()

        result.startTest(self)

        testMethod = self.runTest()
        try:
            outcome = _Outcome()
            self._outcomeForDoCleanups = outcome

            self._executeTestPart(self.setUp, outcome, False)
            if outcome.success:
                self._executeTestPart(testMethod, outcome, True)
                self._executeTestPart(self.tearDown, outcome, False)

            self.doCleanups()
            if outcome.success:
                result.addSuccess(self)
            else:
                for exc_info in outcome.errors:
                    result.addError(self, exc_info)
                for exc_info in outcome.failures:
                    result.addFailure(self, exc_info)
                if outcome.unexpectedSuccess is not None:
                    try:
                        addUnexpectedSuccess = result.addUnexpectedSuccess
                    except AttributeError:
                        addUnexpectedSuccess = None
                    if addUnexpectedSuccess is not None:
                        addUnexpectedSuccess(self)
                    else:
                        result.addFailure(self, outcome.unexpectedSuccess)

                if outcome.expectedFailure is not None:
                    try:
                        addExpectedFailure = result.addExpectedFailure
                    except AttributeError:
                        addExpectedFailure = None
                    if addExpectedFailure is not None:
                        addExpectedFailure(self, outcome.expectedFailure)
                    else:
                        result.addSuccess(self)

        finally:
            result.stopTest(self)
            if orig_result is None:
                try:
                    stopTestRun = result.stopTestRun
                except:
                    stopTestRun = None
                if stopTestRun is not None:
                    stopTestRun()

    def doCleanups(self):
        """Execute all cleanup functions. Normally called for you after
        tearDown."""
        outcome = self._outcomeForDoCleanups or _Outcome()
        while self._cleanups:
            function, args, kwargs = self._cleanups.pop()
            part = lambda: function(*args, **kwargs)
            self._executeTestPart(part, outcome)

        # return this for backwards compatibility
        # even though we no longer us it internally
        return outcome.success

    def fail(self, *args):
        """Fail immediately, with the given message."""
        if len(args) == 0:
            msg = None
        else:
            msg = args[0]
        raise self.failureException(msg)

    def assertFalse(self, *args):
        """Check that the expression is false."""
        try:
            expr = args[0]
            msg = args[1]
        except:
            msg = None

        if expr:
            raise self.failureException(msg)

    def assertTrue(self, *args):
        """Check that the expression is true."""
        try:
            expr = args[0]
            msg = args[1]
        except:
            msg = None

        if not expr:
            raise self.failureException(msg)

    def assertRaises(self, e, f, *args):
        """Fail unless an exception of class excClass is thrown
           by callableObj when invoked with arguments args and keyword
           arguments kwargs. If a different type of exception is
           thrown, it will not be caught, and the test case will be
           deemed to have suffered an error, exactly as for an
           unexpected exception.

           If called with callableObj omitted or None, will return a
           context object used like this::

                with self.assertRaises(SomeException):
                    do_something()

           The context manager keeps a reference to the exception as
           the 'exception' attribute. This allows you to inspect the
           exception after the assertion::

               with self.assertRaises(SomeException) as cm:
                   do_something()
               the_exception = cm.exception
               self.assertEqual(the_exception.error_code, 3)
        """
        try:
            f(*args)
        except e as the_exn:
            return

        raise self.failureException(None)

    def assertEqual(self, first, second):
        if not first == second:
            raise self.failureException(None)

    def assertNotEqual(self, first, second):
        if not first != second:
            raise self.failureException(None)

    def assertIn(self, member, container):
        if member not in container:
            self.fail("assertIn Error")

    def assertNotIn(self, member, container):
        if member in container:
            self.fail("assertNotIn Error")

    def assertIs(self, expr1, expr2):
        if expr1 is not expr2:
            self.fail("assertIs Error")

    def assertIsNot(self, expr1, expr2):
        if expr1 is expr2:
            self.fail("assertIsNot Error")

    def assertIsInstance(self, obj, cls):
        if not isinstance(obj, cls):
            self.fail("assertIsInstance Error")

    def assertNotIsInstance(self, obj, cls):
        if isinstance(obj, cls):
            self.fail("assertNotIsInstance Error")


class TestProgram(object):
    """A command-line program that runs a set of tests; this is primarily
       for making test modules conveniently executable.
    """
    # defaults for testing
    failfast = catchbreak = buffer = progName = warnings = None

    def __init__(self)
        module='__main__'
        if isinstance(module, str):
            self.module = __import__(module)
            for part in module.split('.')[1:]:
                self.module = getattr(self.module, part)
        else:
            self.module = module

        self.exit = None
        self.failfast = None
        self.catchbreak = None
        self.verbosity = 1
        self.buffer = None
        self.warnings = None
        self.defaultTest = None
        self.testRunner = None
        self.testLoader = loader.defaultTestLoader
        self.progName = os.path.basename(argv[0])
        self.runTests()

    def createTests(self):
        if self.testNames is None:
            self.test = self.testLoader.loadTestsFromModule(self.module)
        else:
            self.test = self.testLoader.loadTestsFromNames(self.testNames,
                                                           self.module)

    def runTests(self):
        if self.testRunner is None:
            self.testRunner = runner.TextTestRunner
        testRunner = self.testRunner
        self.result = testRunner.run(self.test) # TODO, create self.test before
        if self.exit:
            sys.exit(not self.result.wasSuccessful())

main = TestProgram
