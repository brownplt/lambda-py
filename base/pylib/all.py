# Return True if all elements of the iterable are true (or if the iterable is empty).
# TypeError will be raised by the for statement if not iterable.
def all(iterable):
    for x in iterable:
        if not x:
            return False
    return True

___assign("%all", all)
