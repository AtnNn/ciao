:- module(global_vars, [setval/2, getval/2, current/2], [hiord]).
:- use_package(assertions).

:- doc(nodoc, assertions).
:- doc(nodoc, hiord).

:- doc(title, "Backtrackable Global Variables").
:- doc(author, "Jose F. Morales").
:- doc(author, "R@'{e}my Haemmerl@'{e}").

:- doc(module, 
"This module provides a simple way to assign and read fully
 backtrackable global variables. Global variables differ from storing
 information using assertz_fact/1.
 @begin{itemize}
   @item The value lives on the Prolog (global) stack. This implies
     that lookup time is independent from the size of the term. This
     is particularly interesting for large data structures such as
     parsed XML documents.
   @item They support only backtrackable assignment using
     @pred{setval/2}. 
   @item Only one value (which can be an arbitrary complex Prolog
     term) can be associated to a variable at a time. 
   @item Global variables are scoped lobally to module. In particular
     a variable create in one module cannot be access from outside.
  @end{itemize}

 Both @pred{getval/2} and @pred{setval/2} implicitly create a
 variable if the referenced names does not
 already refer to a variable in the current module. 
 Notice that the creation of global variable is not undone on 
 backtrack.

Notice that the current implementation has some limitations:
@begin{itemize}
@item No more than 255 modules can use global variables in a program.
@item No more than 255 global variables can be used in a module.
@end{itemize}
"  ).


:- use_module(engine(internals), ['$global_vars_get'/2]).

:- data(module_counter/1).
:- data(key_counter/3).
:- data(key/4).

module_counter(1).

get_info(Module, Key, I, J):-
	key(Module, Key, I, J), !.
get_info(Module, Key, I, J):-
	retract_fact(key_counter(Module, I, J)), !, 
	K is J + 1, assertz_fact(key_counter(Module, I, K)),
	assertz_fact(key(Module, Key, I, J)).
get_info(Module, Key, I, 1):-
	retract_fact(module_counter(I)), 
	J is I + 1, assertz_fact(module_counter(J)), 
	assertz_fact(key_counter(Module, I, 2)), 
	assertz_fact(key(Module, Key, I, 1)).
	
:- meta_predicate  setval(addmodule, +).
:- meta_predicate  getval(addmodule, +).
:- meta_predicate  current(addmodule, +).

:- use_module(library(mutables)).

global(Module, Key, Value):-
	get_info(Module, Key, I, J),
	'$global_vars_get'(3, GlobalArray),
	functor(GlobalArray, t, 255), 
	arg(I, GlobalArray, Array), 
	functor(Array, t, 255), 
	arg(J, Array, Value).

:- pred setval(Name,Value) # "Associate the term @var{Value} with the
atom @var{Name}. If @var{Name} does not refer to an exisitng global
variable, a unbounded global variable @var{Value} is created an unify
to @var{Value}. On backtracking the assignement is reversed. If
@var{Name} is not a atom the predicate silently fails.".

setval(Key, _Module, _Value):-
	\+ atom(Key), !, fail.
setval(Key, Module, Value):-
	global(Module, Key, Mutable), 
	set(Mutable, Value). 

:- pred getval(Name,Value) # "Unfifies @var{Value} with the current value of
the global variable refered by the atom @var{Name}. If @var{Name} does
not refer to an exisiting global variable, a free unbound variable is
created and unify with @var{Value}. If @var{Name} is not a atom
the predicate silently fails.".

getval(Key, _Module, _Value):-
	\+ atom(Key), !, fail.
getval(Key, Module, Value):-
	global(Module, Key, Mutable),
	get(Mutable, Value).

:- pred current(Name,Value) # "Enumerate all defined variables with
their value. The order of enumeration is undefined.".

current(Key, Module, Value):-
	global(Module, Key, Mutable),
	get(Mutable, Value).


set(Mutable, Value):-
	(
	    mutable(Mutable)
	->
	    update_mutable(Value, Mutable)
	;
	    create_mutable(Value, Mutable)
	).


get(Mutable, Value):-
	(
	    mutable(Mutable)
	->
	    get_mutable(Value, Mutable)
	;
	    create_mutable(Value, Mutable)
	).