if 0 < 1 <= 1 == 1 >= 1 > 0 != 1: pass
else: raise Exception('int comparisons failed')
if 0.0 < 1.0 <= 1.0 == 1.0 >= 1.0 > 0.0 != 1.0: pass
else: raise Exception('float comparisons failed')
if '' < 'a' <= 'a' == 'a' < 'abc' < 'abd' < 'b': pass
else: raise Exception('string comparisons failed')
if None is None: pass
else: raise Exception('identity test failed')
