While λπ handles much of Python, some of its most immediate uses involve
wrangling Pythonic scope.

The first two phases of desugaring are related to scope, and for many
applications may produce a more usable AST structure.  The goal of these two
phases is to determine the relationship between the various definition forms
and their use sites.  The definition forms and related modifiers we're
concerned with are:

    x = e

    class C(superclass):
      ...

    def f(arg, ...):
      ...

    global x

    nonlocal x


The scope desugaring takes these forms and elaborates them into a simpler AST
datatype, where each variable is bound with an explicit, lexical, let-binding
form, and each use of a variable is marked with whether it is global or local.
Further, not all assignment statements are actually assignments to variables
directly; some turn into assignments to class members when in the body of a
class.  These assignments are also transformed.

This is accomplished in two phases:

1. Mark each assignment statement and variable use as either global, local,
   or a class instance variable.  The distinction between nonlocal and local is
   removed at this point, as all variables have an obvious, unambiguous binding
   position inserted as a "let"-like form wrapping their use at the appropriate
   level.  Variables that are declared in classes and act as both variables and
   fields of a class are marked as "instance" variables.
2. Transform instance variable assignments to class field assignments, and do
   appropriate lifting of definitions out of classes.

There is a printer for this simplified language and some scripts for
experimenting with it.  Here is an example (run from the base/ directory of a
lambda-py checkout).

Our sample program is:

    $ cat ../examples/scope/nonlocal-function-vardef.py
    def f():
        x = 0
        def inc():
            nonlocal x
            x += 1
            return x
        def dec():
            nonlocal x
            x -= 1
            return x
        return inc, dec

    inc, dec = f()
    ___assertEqual(inc(), 1)
    ___assertEqual(inc(), 2)
    ___assertEqual(dec(), 1)
    ___assertEqual(dec(), 0)

To see the phase-1 desugaring, we can use the script `show-scope.sh`:

    $ ./show-scope.sh < ../examples/scope/nonlocal-function-vardef.py 
    # assigned to (global) f
    def f(    ):
      {
        defvar (local)  x = UNDEF in {
          defvar (local)  inc = UNDEF in {
            defvar (local)  dec = UNDEF in {
              (local) x = 0
              # assigned to (local) inc
              def inc(    ):
                {
                  nonlocal x
                  (local) x+=1
                  return (local) x
                }
              # assigned to (local) dec
              def dec(    ):
                {
                  nonlocal x
                  (local) x-=1
                  return (local) x
                }
              return [(local) inc, (local) dec]
            }
          }
        }
      }
    [(global) inc, (global) dec] = (global) f()
    (global) ___assertEqual((global) inc(), 1)
    (global) ___assertEqual((global) inc(), 2)
    (global) ___assertEqual((global) dec(), 1)
    (global) ___assertEqual((global) dec(), 0)

Notice a few things here:

1.  The local definitions for `x`, `inc`, and `dec` have been explicitly lifted
    to the top of the block of `f`.
2.  The definition of `f` is annotated with a note that it is a global definition
3.  The various *uses* of `x` inside `inc` and `dec` are labelled `local`
    (though, as a reminder, there is a note that there was a nonlocal declaration
    for them).
4.  Both `inc` and `dec` are `global` variables in the global block because of
    the later assignment, but both names are also noted as `local` in the body
    of `f`.

What's printing here is actually a prettier representation of a modified AST
that contains scope-type information explicitly on each use of an identifier.

If we change the program slightly, we can see some differences.  For example,
we could change `dec` to take an `x` parameter, and drop the `nonlocal x` at
the top of `dec`:

    $ cat ../examples/scope/nonlocal-function-vardef-shadow.py 
    def f():
        x = 0
        def inc():
            nonlocal x
            x += 1
            return x
        def dec():
            x = 1 # <--- this line changed
            x -= 1
            return x
        return inc, dec

    inc, dec = f()
    ___assertEqual(inc(), 1)
    ___assertEqual(inc(), 2)
    ___assertEqual(dec(1), 0)  # <--- these calls each have a fresh `x`
    ___assertEqual(dec(1), 0)  # <--/
    ___assertEqual(inc(), 3)


This desugars differently:


    $ ./show-scope.sh < ../examples/scope/nonlocal-function-vardef-shadow.py 
    # assigned to (global) f
    def f(    ):
      {
        defvar (local)  x = UNDEF in {
          defvar (local)  inc = UNDEF in {
            defvar (local)  dec = UNDEF in {
              (local) x = 0
              # assigned to (local) inc
              def inc(    ):
                {
                  nonlocal x
                  (local) x+=1
                  return (local) x
                }
              # assigned to (local) dec
              def dec(    ):
                {
                  defvar (local)  x = UNDEF in {  # <-- new local binding here, shadowing the outer `x`
                    (local) x = 1
                    (local) x-=1
                    return (local) x
                  }
                }
              return [(local) inc, (local) dec]
            }
          }
        }
      }
    [(global) inc, (global) dec] = (global) f()
    (global) ___assertEqual((global) inc(), 1)
    (global) ___assertEqual((global) inc(), 2)
    (global) ___assertEqual((global) dec(), 0)
    (global) ___assertEqual((global) dec(), 0)
    (global) ___assertEqual((global) inc(), 3)


Here we see that in the body of `dec`, there is an additional local binding for
`x` added, and that is the binding used and updated in the body of `dec` (the
other `x` is unaffected).

There is a second step for handling classes, which come with their own
complications.  This is a little more verbose, but after, all variables are
either global or local, and instance variables are turned into object access
and update.  After the first step, they are merely labelled as "instance"
variables, which is still useful for distinguishing their role.  For example,
in this simple class, the binding site of `x` in C is labelled as `instance`.
The `x` in the body of the method `f` is correctly labelled as a reference to
the global `x`, and the final `x` is labelled as local, meaning it will act as
a reference to the nearest (non-global) definition.


    $ cat ../examples/scope/simple-class.py
    x = 5
    class C(object):
      x = 10
      def f(self):
        return x
      y = x + 10

    c = C()
    ___assertEqual(c.y, 20)
    ___assertEqual(c.f(), 5)


    $ ./show-scope.sh < ../examples/scope/simple-class.py 
    (global) x = 5
    class C((global) object):
      {
        (instance) x = 10
        # assigned to (instance) f
        def f((local) self    ):
          {
            nonlocal self
            return (global) x
          }
        (instance) y = (local) x + 10
      }
    (global) c = (global) C()
    (global) ___assertEqual((global) c.y, 20)
    (global) ___assertEqual((global) c.f(), 5)


The full desugaring of the class is more verbose, and arguably harder to relate
to surface Python, but also makes the binding structure totally clear:


    $ ./show-postclass-scope.sh < ../examples/scope/simple-class.py 
    module:
      pass
      defvars x, C, (global) c = UNDEF in {
        (global) x = 5
        # assigned to (global) C
        class C((global) object):
          {
            defvar (local)  class-replacement4308 = UNDEF in {
              # assigned to (local) class-replacement4308
              # Here we make a *function* that refers to the correct x in the
              # scope outside this class body.  This function will be
              # substituted for uses of "x" inside the methods of the class.
              # This works in general for nested combinations of global and
              # nonlocal
              def class-replacement4308((local) self    ):
                {
                  # active locals: self, 
                  nonlocal self
                  return (global) x
                }
              {
                ## Here, the "x" inside the class body is bound to a normal,
                ## let-bound identifier
                defvar (local)  x = UNDEF in {
                  defvar (local)  f = UNDEF in {
                    defvar (local)  y = UNDEF in {
                      # active locals: x, f, y, 
                      ## x is updated, and C's x field is immediately updated to
                      ## the same value: this assignment statement has a dual
                      ## purpose
                      (local) x = 10
                      (global) C.x = (local) x
                      # assigned to (local) f
                      def f((local) self    ):
                        {
                          # This is the use of the function that refers to x
                          return (local) class-replacement4308((local) self)
                        }
                      (global) C.f = (local) f
                      ## Here, the reference to x is a simple, local reference
                      ## to the let-bound x in the class body
                      (local) y = (local) x + 10
                      (global) C.y = (local) y
                    }
                  }
                }
              }
            }
          }
        (global) c = (global) C()
        (global) ___assertEqual((global) c.y, 20)
        (global) ___assertEqual((global) c.f(), 5)
      }

Here, we have re-organized things so that all scope is explicit.  At this
point, the semantics of scope works with simple lexical rules of let-binding
and function parameter passing, and it's explicit which assignments cause
updates to the fields of C, and which nested uses refer to outside the
function.

From here, tools that would be potentially easier to build than on surface
Python's AST are:

1.  A way to get this information into a Python TreeVisitor, so the visitor can
    know the true binding position of each used variable as it encounters them.
2.  A proof-of-concept variable rename refactoring or similar tool, either
    standalone or on top of (1)
3.  A simple type-checker that knows about class fields as both variables and
    members of the class object

One thing the current system lacks is a good cross-module scope story, but this
can be built up from the knowledge of globals in each module and more attention
to import statements.  In easy cases, this is merely a matter of engineering,
and is a direction we'd like to move in.


