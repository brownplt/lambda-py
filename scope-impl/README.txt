Tests: 

So far we pass 88 of 90 tests.

Come on, give a smile.

Design: 

About Desugaring:

	We have a near to 1-on-1 correspondence of PyExpr's and structures from the AST python specification.
	Our desugarer has two important functions: get-vars and desugar.
	Desugar works just like the ones used in the Parseltongue assignments, although a bit more of the work was done at the interpreter.
	get-vars lifts variables just as done in the parseltongue-extended-javascript assignment. However, instead of just getting the variables, 
it also gets the ScopeTypes that each variable should have (Local, NonLocal or Global).

About our Core Language:

	We have a few primitive types (VNum, VStr, VTrue, VNone, VPass...), VUnbound for the situation in which a variable has been defined but not assigned,
and VHash: maybe the most important type. It represents objects (and lists, tuples, sets, dicts and classes are dealt as objects in our language).
	A VHash has a box with hashs from CVals to CVals (because an object is basically a dictionary), a uid (unique identifier) that we use to check the 'is' 
operator, and a Type (with the name and the object representing its class). The basetype of an instance object is the class from which it is created, 
and the basetype of an object class is the one from which it inherits. Every class, at the end, inherits from _Object. And every object, when is created, 
executes the __init__ method in itself.

About our scope:

After analyzing how python scope works, we chose to implement ours with an environment and a store. Our store is just as the one taught in class, 
but our environment goes from an identifier to a (ScopeType * Location), with ScopeType being either 'Local', 'NonLocal' or 'Global'. 
This information is useful because there's a lot of perks related to this in python.

How Scope Works:

Our environment goes from identifier to (type, location), where type can be:
1)Local, 2)Global, 3)NonLocal

Besides the 'regular' environment, that we pass along with the store in interp-env, we have a global variable 'globalEnv' that keeps the 'global environment'.
When in the top-level scope, every variable is kept as Global. This is done by desugaring PyModule.

When creating a new scope, we do it by the following steps. (look at the 'newEnvScope' function to see the code)

1) we add all of the variables in the older scope that are not of the type 'global'. We change the ones that have 'local' tags to 'nonlocal'. The 'global' ones don't need to be copied.

2) we get a list of the global declarations inside of our body. We check if this variable is already in the global environment. If it is, we just put it in our current environment, overwriting it if there's already a variable with the same identifier but different type. If the variable is not yet in the global environment, we add it and then do the same process in the current environment.

3) We get a list of the nonlocal declarations inside of our body. If our new environment already has one of them with a 'global' type, we throw an error. We also throw an error if our current environment does not have this variable (this either means we don't have it or the last time we used it was global). Else, we just keep the list, without changing the environment.

4) we add all of the variables that are assigned inside of our body, and that ARE NOT IN THE GLOBAL/NONLOCAL lists, with the 'local' tag. We check if any of the parameters is in the globals/nonlocal list, and if it is, we throw an error. Then we add the parameters of the function with the 'local' tag.

5) to search for a variable, we first check our environment. If it is there, ok. If not there, search the global environment. To mutate a variable, if it is in the environment, ok. If not, thrown an error.

One important thing to notice is function application. In the last assignments, we just created the new environment during the application. Here, we create it 
when creating the closure. Because of this, we need to allocate a new position for all of the local variables when we apply the function.

About the 2 tests not passing:
  Both of them are in Scope.
  freevar-in-method does not work because we didn't expect, when first designing our scope, all of the specific nuances of scope in class definitions. 
  We have put some significant effort into trying to fix this, and if we do, we will probably re-submit the assignment. 
  local-functions does not work because we assumed that local variables should be the ones that are 1)assigned in the scope block, 2)names of functions declared 
in the scope block, 3)names of classes defined in the scope block or 4) arguments of the function. However, it seems that if a variable is just used, it should 
be also a Locals.

The file "Lib" contains the definitions of primitive classes corresponding to tuples, lists, sets, dicts, and the other primitive datatypes. 
It also holds a lot of Python functions (range, filter, etc). 

Exceptions and breaks/continues/returns are handled in a manner similar to that used in parselextend - there is a variant type to AnswerC for 
each of them, and on receipt of those variants we perform the ncessary operation. For instance, an exception should be passed through a function
call, but would be caught by an TryExcept. 

We've used iterators for a lot of functionality, including string slices. 