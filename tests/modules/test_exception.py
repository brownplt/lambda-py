def foo():
    import support_exception

___assertRaises(TypeError, foo)
#try:
#    import support_exception
#    ___assertTrue(False)
#except TypeError("foo"):
#    pass
#else:
#    ___assertTrue(False)
