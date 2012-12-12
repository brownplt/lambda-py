def make_adder4(): # XXX add exta level of indirection
    def nest():
        def nest():
            def adder(y):
                return global_x + y # check that plain old globals work
            return adder
        return nest()
    return nest()

global_x = 1
adder = make_adder4()
___assertEqual(adder(1), 2)

global_x = 10
___assertEqual(adder(-2), 8)
