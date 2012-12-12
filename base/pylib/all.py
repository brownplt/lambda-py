def all(l):
    if not isinstance(l, list):
        raise TypeError("not a list")
    for x in l:
        if not x:
            return False
    return True



