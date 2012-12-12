def ___assertRaises(exc, fun, *args):
    try:
        fun(*args)
    except exc as e:
        pass 
    except:
        print("Assert failure!")
    else:
        print('Assert failure!')
