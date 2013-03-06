x = 7
def f():
  x = 1
  def g():
    global x
    def i():
      def h():
        return x
      return h()
    return i()
  return g()

___assertEqual(f(), 7)
___assertEqual(x, 7)

