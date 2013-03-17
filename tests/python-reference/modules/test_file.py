source = "a.txt"
f = open(source, "w")
f.write("hello\n")
f.write("world\n")
f.close()

f = open(source, "r")
___assertEqual(f.read(), "hello\nworld\n")
___assertEqual(f.read(), "")
f.close()

f = open(source, "r")
___assertEqual(f.readline(), "hello\n")
___assertEqual(f.readline(), "world\n")
f.close()
