try:
  raise Exception('just testing scope')
except Exception as e:
  ___assertTrue(isinstance(e, Exception))
