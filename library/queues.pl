:- module(queues, [q_empty/1, q_insert/3, q_member/2, q_delete/3],
	[assertions]).

q_empty(X-X).

q_insert(E, H-[E|T], H-T).

q_member(E, H-_) :-
        nonvar(H),
        H = [X|Xs],
        il_member(E,X,Xs).

il_member(E, E, _).
il_member(E, _, L) :-
        nonvar(L),
        L = [X|Xs],
        il_member(E,X,Xs).

q_delete(E, H-T, H1-T) :-
        nonvar(H),
        H = [X|Xs],
        il_delete(E,X,Xs,H1).

il_delete(E, E, L, L).
il_delete(E, Y, L, [Y|L1]) :-
        nonvar(L),
        L = [X|Xs],
        il_delete(E,X,Xs,L1).
     

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

