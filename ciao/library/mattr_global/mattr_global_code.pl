:- module(mattr_global_code, [get_attr/3, set_attr/3, detach_attr/2],
	    [assertions]).

:- use_module(engine(attributes)).


%get_attr
get_attr(X, A, user(_)) :-
	!,
	get_attr(X, A, user).

get_attr(X, ModuleAttr, Name) :-
	attributes:get_attribute(X, simple_attr(Attr)),
	Find =.. [Name, ModuleAttr],
	member(Find, Attr).



%% Insert in order. If it exists, replace it

% the same => rewrite
internal_insert_attr(A, [P|R], [A|R]) :-
	functor(A, N, _),
	functor(P, N, _),
	!.

% less => insert
internal_insert_attr(A, [P|R], [A, P|R]) :-
	A @< P,
	!.

% keep it
internal_insert_attr(A, [P|R], [P|LR]) :-
	internal_insert_attr(A, R, LR),
	!.

% last or empty => add
internal_insert_attr(A, [], [A]).


%% Delete in order.

internal_delete_attr(A, [P|R], RES) :-
	functor(A, FA, _),
	functor(P, FP, _),
	(
	    FA @< FP
	->
% less => continue
	    RES = [P|RET],
	    internal_delete_attr(A, R, RET)
	;
% the same => delete it
	    FA == FP,
	    RES = R
	),
	!.


% A @> P or last or empty => it was no here
internal_delete_attr(_, A, A).


% Things that can happend:
% 1. Module 'Name has never set an attribute to variable X
% 2. Attribute is locked, so 2nd arg has to be changed
% 3. Attribute is no locked, so 1st arg has to be changed

set_attr(X, A, user(_)) :-
	!,
	set_attr(X, A, user).


set_attr(X, A, Name) :-
	(
	    attributes:get_attribute(X, simple_attr(Attr))
	->
	    NA =.. [Name, A],
	    internal_insert_attr(NA, Attr, New_AttList),
	    attributes:update_attribute(X, simple_attr(New_AttList))
	;
% It is the first time that module 'Name' sets 
% an attribute to this variable
	    NA =.. [Name, A],
	    (
		var(X) ->
		attributes:attach_attribute(X, simple_attr([NA]))
	    ;
		message(error, ['set_attr: attaching ', A, ' to nonvar ', X])
	    )
	).




detach_attr(X, user(_)) :-
	!,
	detach_attr(X, user).

detach_attr(X, Name) :-
	attributes:get_attribute(X, simple_attr(Attr)),
	NA =.. [Name, _],
	internal_delete_attr(NA, Attr, New_AttList),
	(
	    New_AttList == []
	->
	    attributes:detach_attribute(X)
	;
	    attributes:update_attribute(X,
		simple_attr(New_AttList))
	).



:- multifile portray_attribute/2.

:- multifile '$portray_mattr'/2.

portray_attribute(simple_attr(Attr), _Var) :-
	display_mattr_list(Attr).


display_mattr_list([A]) :-
	A =.. [F, Attr],
	display_functor(F),
	display(' => '),
	(
	    '$portray_mattr'(F, Attr)
	->
	    true
	;
	    display(Attr)
	),
	!.

display_mattr_list([]).

display_mattr_list([A|As]) :-
	display_mattr_list([A]),
	display(' , '),
	display_mattr_list(As).




%display_functor( user ) :-
%	display( user ).

display_functor(X) :-
	atom_codes(X, Codes),
	Codes = [N1, _N2, _N3|Name],
	N1 >= 0'0,
	N1 =< 0'9,
	!,
	atom_codes(AName, Name),
	display(AName).

display_functor(X) :-
	display(X).
