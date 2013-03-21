module = 'utest'
utest = __import__(module)

class TestClass1(utest.TestCase):
    def test_1(self):
        self.assertTrue(True)
    def test_11(self):
        self.assertTrue(False)

class TestClass2(utest.TestCase):
    def test_2(self):
        self.assertTrue(True)
    def test_22(self):
        self.assertTrue(True)
    def test_23(self):
        self.assertRaises(TypeError, list, 0)

utest.main()
