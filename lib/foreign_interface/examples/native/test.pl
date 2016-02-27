:- module(test, [test/1], [assertions, isomodes, 'native_interface/syntax']).

:- impl_defined([i_am_native/1]).

:- true pred i_am_native(-float) + native.

test(X) :-
	i_am_native(X).

:- use_foreign_source(test).


