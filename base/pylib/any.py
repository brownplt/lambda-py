# Return True if any element of the iterable is true. If the iterable is empty, return False.
# TypeError will be raised by the for statement if not iterable.
def any(iterable):
    for x in iterable:
        if x:
            return True
    return False

___assign("%any", any)
