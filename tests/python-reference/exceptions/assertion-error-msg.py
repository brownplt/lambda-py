# test assertion error msg
def assertion_test(test, good, bad):
    try:
        assert test, bad
        result = good
    except AssertionError as e:
        result = e.args[0]
    return result

___assertEqual(assertion_test(True, "Good", "Bad"), "Good")
___assertEqual(assertion_test(False, "Good", "Bad"), "Bad")
