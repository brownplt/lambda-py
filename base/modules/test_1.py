import unittest

class TestClass1(unittest.TestCase):
    def test_1(self):
        self.assertTrue(True)
    def test_11(self):
        self.assertTrue(False)

class TestClass2(unittest.TestCase):
    def test_2(self):
        self.assertTrue(True)
    def test_22(self):
        self.assertTrue(True)
    #def test_23(self):
        #self.assertRaises(TypeError, list, 0)
    def test_24(self):
        self.assertRaises(TypeError, all, 0)

unittest.main()
