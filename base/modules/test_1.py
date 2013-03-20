utest = __import__('utest')

class TestClass1(utest.TestCase):
    def test_1(self):
        self.assertTrue(True)
    def test_11(self):
        self.assertTrue(True)

class TestClass2(utest.TestCase):
    def test_2(self):
        self.assertTrue(True)
    def test_22(self):
        self.assertTrue(True)

utest.main()
