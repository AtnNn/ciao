:- module(foreign_interface_tr, [foreign_interface_tr/3], [assertions]).

:- use_package(assertions).

% bug: the asssertion read without normalization

foreign_interface_tr(Assertion, Decls, _Mod) :-
	is_assertion(Assertion),
	assertion_pred(Assertion, Pred),
	assertion_props(Assertion, Props),
	foreign_prop(Props),
	functor(Pred, Name, Arity),
	Decls = [Assertion, (:- impl_defined(Name/Arity))].

is_assertion((:- _ pred _)).

assertion_pred((:- _ pred Pred + _ # _), Pred) :- !.
assertion_pred((:- _ pred Pred # _), Pred) :- !.
assertion_pred((:- _ pred Pred :: _), Pred) :- !.
assertion_pred((:- _ pred Pred + _), Pred) :- !.
assertion_pred((:- _ pred Pred), Pred).

assertion_props((:- _ pred _ :: _ + Props # _), Props) :- !.
assertion_props((:- _ pred _ :: _ + Props), Props) :- !.
assertion_props((:- _ pred _ + Props # _), Props) :- !.
assertion_props((:- _ pred _ + Props), Props) :- !.

foreign_prop(foreign(_)).
foreign_prop(foreign).
foreign_prop(native(_)).
foreign_prop(native).
foreign_prop((A, B)) :- ( foreign_prop(A) -> true ; foreign_prop(B) ).

:- comment(version_maintenance,dir('../../version')).


:- comment(version(1*9+63,2003/02/28,16:42*43+'CET'), "Added a
   sentence translation to automatically insert a impl_defined/1
   declaration for each foreign predicate.  (Jose Morales)").

