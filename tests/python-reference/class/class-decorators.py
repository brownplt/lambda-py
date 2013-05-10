def d1(cls):
    cls.dec_attr = "d1"
    return cls

# Decorator d1 adds a new attribute
@d1
class C1:
    pass

___assertEqual(C1.dec_attr, "d1")

def d2(cls):
    try:
        cls.dec_attr = cls.dec_attr + "d2"
    except:
        cls.dec_attr = "d2"
    return cls

# Decorator d2 adds a new attribute
@d2
class C2:
    pass

___assertEqual(C2.dec_attr, "d2")

# Attribute already exists, added by decorator d1
@d2
@d1
class C12:
    pass

___assertEqual(C12.dec_attr, "d1d2")

# Attribute already exists in the class body
@d2
class C2body:
    dec_attr = "C"

___assertEqual(C2body.dec_attr, "Cd2")
