source = "a.txt"
f = open(source, "w")
f.write("hello\n")
f.write("world\n")
f.close()

# no exceptions, the file should be closed on exit from with statement
with open(source, "r") as f:
    ___assertEqual(f.readline(), "hello\n")

___assertRaises(ValueError, f.readline)


# exception inside the with block should be propagated and the file closed
try:
    with open(source, "r") as f:
        raise Exception(f.readline())
        ___fail()
except Exception as e:
    ___assertEqual(e.args[0], "hello\n")
    ___assertRaises(ValueError, f.readline)
else:
    ___fail()
