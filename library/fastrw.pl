:- module(fastrw,
        [fast_read/1,
         fast_write/1,
         fast_write_to_string/3],
         [dcg,assertions]).

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

:- comment(version(0*0+0,1998/2/23), "Added inclusion of dcg syntax
   lib. (Manuel Hermenegildo)").

:- use_module(library(dict)).

version(0'C).

fast_read(X) :-
        get_code(V),
        version(V),
        get_code(C),
        fast_read_(C,_Dict, X).

fast_read_(0'],_Dict, []).
fast_read_(0'_, Dict, V) :-
        get_null_ended(S, []),
        number_codes(N,S),
        dic_lookup(Dict, N, V).
fast_read_(0'I,_Dict, I) :-
        get_null_ended(S, []),
        number_codes(I,S).
fast_read_(0'F,_Dict, F) :-
        get_null_ended(S, []),
        number_codes(F,S).
fast_read_(0'A,_Dict, A) :-
        get_null_ended(S, []),
        atom_codes(A,S).
fast_read_(0'", Dict, S) :-      %" Make CIAO mode think the string has ended
        get_null_ended(S, T),
        get_code(C),
        fast_read_(C, Dict, T).
fast_read_(0'[, Dict, [X|Xs]) :-
        get_code(C),
        fast_read_(C, Dict, X),
        get_code(D),
        fast_read_(D, Dict, Xs).
fast_read_(0'S, Dict, Str) :-
        get_null_ended(S, []),
        get_code(A),
        atom_codes(F, S),
        functor(Str, F, A),
        fast_read_args(1, A, Dict, Str).

fast_read_args(A, N, Dict, Str) :- A =< N, !,
        arg(A, Str, X),
        get_code(C),
        fast_read_(C, Dict, X),
        A1 is A+1,
        fast_read_args(A1, N, Dict, Str).
fast_read_args(_, _, _, _).

get_null_ended(S, T) :-
        get_code(C),
        get_null_ended_(C, S, T).

get_null_ended_(0, T, T) :- !.
get_null_ended_(C, [C|S], T) :-
        get_code(C1),
        get_null_ended_(C1, S, T).

fast_write(T) :-
        version(V),
        put_code(V),
        fast_write_(T,_Dict).

fast_write_(V, Dict) :-
        var(V), !,
        index_of(Dict, V, N),
        put_code(0'_),
        display(N),
        put_code(0).
fast_write_(I, _) :-
        integer(I), !,
        put_code(0'I),
        display(I),
        put_code(0).
fast_write_(F, _) :-
        float(F), !,
        put_code(0'F),
        display(F),
        put_code(0).
fast_write_([], _) :- !,
        put_code(0']).
fast_write_(A, _) :-
        atom(A), !,
        put_code(0'A),
        display(A),
        put_code(0).
fast_write_([X|Xs], Dict) :-
        integer(X), X > 0, X =< 255, !,
        put_code(0'"),
        put_code(X),
        fast_write_string(Xs, Dict).
fast_write_([X|Xs], Dict) :- !,
        put_code(0'[),
        fast_write_(X, Dict),
        fast_write_(Xs, Dict).
fast_write_(Str, Dict) :-
        put_code(0'S),
        functor(Str,F,A),
        display(F),
        put_code(0),
        put_code(A),
        fast_write_args(1, A, Str, Dict).

fast_write_args(A, N, Str, Dict) :- A =< N, !,
        arg(A, Str, X),
        fast_write_(X, Dict),
        A1 is A+1,
        fast_write_args(A1, N, Str, Dict).
fast_write_args(_, _, _, _).

fast_write_string([X|Xs], Dict) :-
        integer(X), X > 0, X =< 255, !,
        put_code(X),
        fast_write_string(Xs, Dict).
fast_write_string(T, Dict) :-
        put_code(0),
        fast_write_(T, Dict).

index_of(Dict, T, N) :- index_of_(Dict, T, 0, N).

index_of_(V, T, I, N) :-
        var(V), !,
        V = [T|_], N = I.
index_of_([T0|_], T, I, N) :-
        T0 == T, !,
        N = I.
index_of_([_|D], T, I, N) :-
        I1 is I+1,
        index_of_(D, T, I1, N).

fast_write_to_string(T, S, R) :-
        version(V),
        S = [V|S_],
        fastrw_term(T,_Dict, S_, R).

fastrw_term(V,Vdict) --> {var(V)}, !,
        "_",
        {index_of(Vdict, V, N), number_codes(N,S)},
        string(S), [0].
fastrw_term(I,_) --> {integer(I)}, !,
        "I",
        {number_codes(I,S)},
        string(S), [0].
fastrw_term(F,_) --> {float(F)}, !,
        "F",
        {number_codes(F,S)},
        string(S), [0].
fastrw_term([],_) --> !,
        "]".
fastrw_term(A,_) --> {atom(A)}, !,
        "A",
        {atom_codes(A,S)},
        string(S), [0].
fastrw_term([X|Xs],Vdict) --> {integer(X), X > 0, X =< 255}, !,
        """",
        [X],
        fastrw_string(Xs, Vdict).
fastrw_term([X|Xs],Vdict) --> !,
        "[",
        fastrw_term(X,Vdict),
        fastrw_term(Xs,Vdict).
fastrw_term(S,Vdict) --> {functor(S,F,A)},
        "S",
        {atom_codes(F,C)},
        string(C), [0,A],
        fastrw_args(S, 1, A, Vdict).

fastrw_args(S,I,N,Vdict) --> {I =< N}, !,
        {arg(I,S,T)},
        fastrw_term(T,Vdict),
        {I1 is I+1},
        fastrw_args(S,I1,N,Vdict).
fastrw_args(_,_,_,_) --> "".

fastrw_string([X|Xs],Vdict) --> {integer(X), X > 0, X =< 255}, !,
        [X],
        fastrw_string(Xs,Vdict).
fastrw_string(T,Vdict) -->
        [0],
        fastrw_term(T,Vdict).

string([]) --> "".
string([C|Cs]) -->
        [C],
        string(Cs).

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

