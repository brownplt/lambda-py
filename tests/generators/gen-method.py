# This test case is modified from the official python website

def g(value = None):
	while True:
		try:
			value = (yield value)
		except TypeError:
			value = "TypeError"

gen1 = g()

___assertEqual(next(gen1), None)

# resume the execution and sends a value into the generator functions
# the "value" arguments becomes the result of the yield expression
___assertEqual(gen1.send(173),173)

# raises an exception at the point where generator was paused
___assertEqual("TypeError", gen1.throw(TypeError))

# exit the Generator
gen1.close()
___assertRaises(StopIteration, gen1.__next__)