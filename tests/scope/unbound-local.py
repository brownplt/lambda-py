# use of variable before assignment. There shouldn't be a
# problem with this case unless we fail to correctly lift
# all variables and assign them to Unbound if they are 
# not defined. 

# TODO: formalize what is going wrong if we get NameError
# instead of UnboundLocalError, or elsewise. 

def errorInOuter():
    print(y)
    def inner():
        return y
    y = 1

def errorInInner():
    def inner():
        return y
    inner()
    y = 1

___assertRaises(UnboundLocalError, errorInOuter)
___assertRaises(NameError, errorInInner)
