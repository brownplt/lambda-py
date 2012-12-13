if 'xyz' + 'abcde' != 'xyzabcde': raise Exception('string concatenation')
if 'xyz'*3 != 'xyzxyzxyz': raise Exception('string repetition *3')
if 0*'abcde' != '': raise Exception('string repetition 0*')
if min('abc') != 'a' or max('abc') != 'c': raise Exception('min/max string')
if 'a' in 'abc' and 'b' in 'abc' and 'c' in 'abc' and 'd' not in 'abc': pass
else: raise Exception('in/not in string')
