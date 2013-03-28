class Foo(object):
    attr1 = "Foo.attr1"
    def __init__(self):
        self.attr2 = "Foo().attr2"

class Bar(Foo):
    attr1 = "Bar.attr1"
    def __init__(self):
        Foo.__init__(self)
        self.attr3 = "Bar().attr3"
    def __dir__(self):
        return ['attr1', 'attr3', 'attr2', 'NO_SUCH_ATTR']

f = Foo()
b = Bar()
# this will set attribute on Foo and Bar and their
# objects
setattr(Foo, 'new_attr', 'f.new_attr')
Foo_dir = dir(Foo)
Bar_dir = dir(Bar)
f_dir = dir(f)
b_dir = dir(b)
___assertIn('new_attr', Foo_dir)
___assertIn('new_attr', Bar_dir) 
___assertIn('new_attr', f_dir)
___assertNotIn('new_attr', b_dir) # Bar() has __dir__
___assertIn('NO_SUCH_ATTR', b_dir) # Bar() has __dir__
___assertIn('attr1', Foo_dir)
___assertIn('attr2', f_dir)

___assertEqual(Foo.new_attr, 'f.new_attr')
___assertEqual(Bar.new_attr, 'f.new_attr')
___assertEqual(getattr(f, 'attr1'), "Foo.attr1")
___assertEqual(getattr(Foo, 'attr1'), "Foo.attr1")
___assertEqual(getattr(f, 'attr2'), "Foo().attr2")
___assertEqual(getattr(b, 'attr3'), "Bar().attr3")
___assertEqual(getattr(b, 'attr2'), "Foo().attr2")
___assertEqual(getattr(b, 'attr1'), "Bar.attr1")

___assertRaises(AttributeError, getattr, f, 'non_attr')

