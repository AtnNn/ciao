%%--------------------------------------------%%
%% class/1, at the beginning of the file,     %%
%% will declare current source to be a class. %%
%%--------------------------------------------%%

:- class(stack,[],[]).

% State declaration: storage/1 will be an attribute.
:- dynamic storage/1.

% Interface declaration: the following predicates will
% be available at run-time.

:- export(push/1).
:- export(pop/1).
:- export(top/1).
:- export(is_empty/0).

% Code 

push(Item) :-
	nonvar(Item), 
	asserta_fact(storage(Item)).

pop(Item) :-
	var(Item),
	retract_fact(storage(Item)).

top(Top) :-
	storage(Top),
	!.

is_empty :-
	storage(_),
	!,
	fail.

is_empty.

:- comment(version_maintenance,off).

