"""Term Project Test Cases"""
import unittest
from term_project import stats
from heaps import min_heap, max_heap


class TestStats(unittest.TestCase): # Inherits the module tools
    def setUp(self):
        self.statistics_app = stats()
    
    def test_case_01(self):
        self.assertEqual(self.statistics_app.get_mean(1), 5.407)
    
    def test_case_02(self):
        self.assertEqual(self.statistics_app.get_median(1), 5.38)
    
    def test_case_03(self):
        self.assertEqual(self.statistics_app.get_std_dev(2), 0.397)
    
    def test_case_04(self):
        self.assertEqual(self.statistics_app.search("Switzerland"), [6, 7.48, 1.452, 1.526, 1.052, 0.572, 0.263, 0.343])
    
    def test_case_05(self):
        self.assertEqual(self.statistics_app.get_top_k(3, 3),[('Iceland', 1.624), ('Finland', 1.587), ('Norway', 1.582)])
    
    def test_case_06(self):
        self.assertEqual(self.statistics_app.get_bottom_k(2, 2), [('Somalia', 0.0), ('Central African Republic', 0.026)])

if __name__ == '__main__': # Notice that we call the unit test main function
    unittest.main()