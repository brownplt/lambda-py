def filter(f, l):

    new_l = []
    if f is None:
        for x in l:
            if x:
                new_l.append(x)
    else:
        for x in l:
            try:
                if f(x):
                    new_l.append(x)
            except TypeError:
                raise TypeError("Arity")

    return new_l


