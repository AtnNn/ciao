:- class(role).

:- export(nickname/1).
:- virtual(nickname/1).
:- data(nickname/1).

nickname(_) :-
	fail. 

role(Name) :-
	atom(Name),
	set_fact(nickname(Name)).

	
