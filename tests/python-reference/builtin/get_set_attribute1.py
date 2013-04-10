class Foo(object):
    attr1 = "Foo.attr1"
    def __init__(self):
        self.attr2 = "Foo().attr2"

f = Foo()
# this will set attribute on Foo and their objects
setattr(Foo, 'new_attr', 'f.new_attr')
Foo_dir = dir(Foo)
f_dir = dir(f)

___assertIn('new_attr', Foo_dir)
___assertIn('new_attr', f_dir)
___assertIn('attr1', Foo_dir)
___assertIn('attr2', f_dir)

___assertEqual(Foo.new_attr, 'f.new_attr')
___assertEqual(getattr(f, 'attr1'), "Foo.attr1")
___assertEqual(getattr(Foo, 'attr1'), "Foo.attr1")
___assertEqual(getattr(f, 'attr2'), "Foo().attr2")

___assertRaises(AttributeError, getattr, f, 'non_attr')
