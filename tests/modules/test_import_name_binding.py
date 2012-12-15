# import x.y.z binds x in the current namespace
import test_as
import test_as as x

___assertTrue(x is test_as)

# import x.y.z as w binds z as w
import test_as.empty
import test_as.empty as y

___assertTrue(y is test_as.empty)
