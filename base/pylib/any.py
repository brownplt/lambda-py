def any(l):
    if not isinstance(l, list):
        raise TypeError("not a list")
    for x in l:
        if x:
            return True
    return False

