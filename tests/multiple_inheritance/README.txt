The initial multiple inheritance test set includes six test cases taken from
test_descr.py and one from super.py in the Lib/test directory of the source
bundle (http://www.python.org/ftp/python/3.2.3/Python-3.2.3.tgz).

1) test_multiple_inheritance.py
    basic multiple inheritance test, includes inheritance from builtin classes
    (dict and list) and uses the one-argument version of type().

2) test_diamond_inheritence.py
    multiple inheritance special cases, including a couple of mro disagreement
    which should fail to create the class with TypeError exception

3) test_ex5_from_c3_switch.py
    ex5 (http://mail.python.org/pipermail/python-dev/2002-October/029035.html)
    which shows inconsistency between 2.2 mro implementation and documentation

4) test_monotonicity.py
    monotonicity check according to
    http://192.220.96.201/dylan/linearization-oopsla96.html

5) test_consistency_with_epg.py
    extended precedence graph check according to
    http://192.220.96.201/dylan/linearization-oopsla96.html

6) test_mro_disagreement.py
    several simple cases and a more complex one from
    http://192.220.96.201/dylan/linearization-oopsla96.html,
    it depends on the three argument version of type().

7) test_super_multiple_inheritance.py: the full version of test_super,
    to test multiple inheritance interaction with super().

The use of unitest was replaced by the facilities included in py-prelude.py in
order to be able to work with the project's test framework.
