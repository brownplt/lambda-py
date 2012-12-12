def range(arg1, *starargs):
    l = []
    end = 0

    if not isinstance(arg1, int):
        raise TypeError("Bad type in range")

    if(len(starargs) == 0):
        i = 0
        while i < arg1:
            l += [i]
            i += 1
        return l

    if(len(starargs) == 1):
        if not isinstance(starargs[0], int):
            raise TypeError("Bad type in range")
        i = arg1
        end = starargs[0]
        while i < end:
            l += [i]
            i += 1

        return l

    if(len(starargs) == 2):
        if not isinstance(starargs[0], int):
            raise TypeError("Bad type in range")
        if not isinstance(starargs[1], int):
            raise TypeError("Bad type in range")
        i = arg1
        end = starargs[0]
        step = starargs[1]

        if step == 0:
            raise ValueError("Step of 0 in range")

        if step > 0:
            while i < end:
                l += [i]
                i += step
        else:
            while i > end:
                l += [i]
                i += step

        return l

    if(len(starargs) > 2):
        raise TypeError("Arity mismatch")






