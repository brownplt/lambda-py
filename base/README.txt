Design:
=========
Our core language is heavily object focused. The only values our core produces
are Objects and Closures. A overview of the major design choices follows. 

Builtin functionality:
In order to create builtin functionality such as
adding numbers or storing things in collections, our Objects carry metadata in
their MetaVal field. This allows us to carry along Racket data that can then be
accessed in the interpreter. Access to this metadata is generally carried out in
CBuiltinPrims, which call into native code in order to produce answers. We
designed our interpreter to avoid this as much as possible and to implement as
much builtin functionality as we could in terms of core code or surface code or
even python that is parsed into surface code. 


Control flow and execution:
Our interp performs the usual recursive descent into the core code to interpret.
We thread both an environment and a store. The threading of the environment was
seen as necessary since changes to the environment can take place within the
same level of the syntax tree, so it is insufficient to pass the environment
through the recursive calls like we did for ParselTongue. Results from an
interpretation are either returns, exceptions, breaks, or an actual value. The
first three generally work in the same fashion where they rise up through the
excecution looking for function calls, try blocks, or loops respectively. 

Scoping:
The environment threaded through the interpreter is implemented as a list of
hashes each of which corresponds to a "level" of scope. A new level is pushed to
this list when we enter the body a function. This allows us to easily handle
shadowing of variables inside functions. In order to accommodate Python's
conception of global, local, and nonlocal scope, we analyze the positioning of
identifiers during desugaring, while looking for global/nonlocal keywords in
order to label the identifiers as such. Thus the core interpreter knows when it
encounters an identifier what scoping rules are being applied to it and performs
the correct type of lookup. This means that a global id will be looked up in the
top level of the environment or nonlocal will be looked for in any but the
latest scope level. 

Objects:
Everything is a VObject and each VObject has an "antecedent". The antecedent for an
object, in the surface-Python sense, has an antecedent which refers to its class,
and for a given class the antecedent refers to its super class. A class also
contains an environment level which corresponds to the environment that was
interpreted in its body. Looking up a field in an object thus looks in the object
itself, then its antecedent, then the antecedent of the antecedent, etc. 

Iterators:
Iterator functionality is mostly implemented in actual Python code in the pylib/
directory. The only core functionality that was added is while loops. For-loops
are desugared as while loops with iterators. 

Code organization:
===================
python-desugar,python-interp,get-structured-python all do what you expect. 

python-primitives.rkt - defines the lookup table for CBuiltinPrim which
dispatches builtin functionality

python-lib.rkt - sets up names for builtin functions and wraps them around user
                code

pylib/ - this directory contains .py files which are loaded up in python-lib and
         wrapped around user code

builtins/ - contains racket files defining classes and functions for builtin types like str,list, etc.
            This is where python-primitives usually dispatches the CBuiltinPrim functionalty



