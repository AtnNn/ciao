:- module(test_all, [main/0, test/1], []).

:- use_module(library(format)).

file(addresses,'addresses/objects').
file(byte_lists,'byte_lists/byte_lists').
file(int_lists,'int_lists/int_lists').
file(math,'math/math').
file(strings_and_atoms,'strings_and_atoms/strings_and_atoms').

test(addresses) :-
	format("Testing addresses:~n~n",[]),
	member(X,[1,2,3,4,5,6,7,8,9]),
	object(X,O),
	format("Object ~w has address ~w.~n",[X,O]),
	format("Showing object (from C)...~n",[]),
	show_object(O),
	fail.
test(addresses).

test(byte_lists) :-
	format("Testing lists of bytes:~n~n",[]),
	member(X,[0,1,2,3,4,5,6,7,8,9,100]),
	byte_lists:obtain_list(X,L,List),
	format("~w is a list of length ~w (= ~w).~n",[List,X,L]),
	format("Showing list (from C)...~n",[]),
	byte_lists:show_list(L,List),
	fail.
test(byte_lists).

test(int_lists) :-
	format("Testing lists of integers:~n~n",[]),
	member(X,[0,10,20,30,40,512]),
        int_lists:obtain_list(X,L,List),
	format("~w is a list of length ~w (= ~w).~n",[List,X,L]),
	format("Showing list (from C)...~n",[]),
	int_lists:show_list(L,List),
	fail.
test(int_lists).

test(math) :-
	format("Testing numbers:~n~n",[]),
	sin(0,X),
	sin(1,Y),
	sin(3.1415,Z),
	format("sin(0)=~w, sin(1)=~w, sin(3.1415)=~w.~n",[X,Y,Z]).

test(strings_and_atoms) :-
	format("Testing strings and atoms:~n~n",[]),
	lookup_string(1,S1),
	lookup_string(2,S2),
	lookup_atom(1,A1),
	lookup_atom(2,A2),
	a_string(S),
	format("The following two lines should be identical.~n",[]),
	format("~w ~w ~w ~w ~w~n",
	       ["bcdefg",bcdefg,"cdefghi",cdefghi,
	        "this is a string Prolog should not free"]),
	format("~w ~w ~w ~w ~w~n",[S1,A1,S2,A2,S]),
	show_string("hello message (Prolog string) from C."),
	show_atom('hello message (Prolog atom) from C.').

:- include(library(foreign_interface)).

:- use_module('addresses/objects').
:- use_module('byte_lists/byte_lists').
:- use_module('int_lists/int_lists').
:- use_module('math/math').
:- use_module('strings_and_atoms/strings_and_atoms').

 %% rebuild_all :-
 %% 	member(X,[addresses,byte_lists,math,strings_and_atoms]),
 %% 	file(X,F),
 %% 	(
 %% 	    rebuild_foreign_interface(F,_) ->
 %% 	    true
 %% 	;
 %% 	    format("failed rebuilding foreign interface for ~w~n",[X])
 %% 	),
 %% 	fail.
 %% rebuild_all.

main:-
        file(X, _),
        test(X),
        fail.
main.
