def gen():
    for i in range(1,1000):
        yield i
        for j in range(0,500):
            yield j
            yield i+j
            
