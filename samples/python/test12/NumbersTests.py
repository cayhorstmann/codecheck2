import unittest, numbers

class NumbersTest(unittest.TestCase):

    def testNonNegativeSquares(self):
        for n in range(100):
            self.assertEqual(n * n, numbers.square(n))

    def testNegativeSquares(self):
        for n in range(1, 100):
            self.assertEqual(n * n, numbers.square(-n))
