:- module(term_ser, []).

:- use_module(term_ser_util, [string/1, string_list/2]).

:- export(term_to_ser/3).
:- export(term_to_ser/4).

term_to_ser(V, L, R):-
        term_to_ser(V, _, L, R).

term_to_ser(V, D, L, R):-
        term_to_ser_1(V, D, L, [10|R]).        

term_to_ser_1(V, D) -->
        {var(V)},
        !,
        {find_var_number(V, D, 1, N)},
        [0'$],
        {number_codes(N, NC)},
        prefix(NC).
term_to_ser_1(V, _) -->
        {number(V)},
        !,
        {number_codes(V, C)},
        prefix(C).
% term_to_ser_1(S, D) -->
%         {string(S)},
%         !,
%         {string_list(S, C)},
%         term_to_ser_1(C, D).
term_to_ser_1([], _) --> 
        !,
        [0'[, 0']].
term_to_ser_1([H|T], D) -->
        {proper_list(T)},
        !,
        [0'[],
        term_to_ser_1(H, D),
        list_to_ser(T, D).
term_to_ser_1('', _) --> !,
        [0'(, 0')].
term_to_ser_1(A, _) -->
        {atom(A)},
        !,
        {atom_codes(A, [C|Cs])},
        (  {C>=0'0, C=<0'9}
        -> [0'\\, C], prefix_atom(Cs)
        ;  prefix_atom([C|Cs])
        ).
term_to_ser_1(S, D) -->
        {S =.. [F|As]},
        {atom_codes(F, C)},
        [0'(],
        prefix_atom(C),
        args_to_ser(As, D).


proper_list(L):-
        nonvar(L),
        (  L=[]
        -> true
        ;  L=[_|R],
           proper_list(R)
        ).

list_to_ser([], _) --> 
        !,
        [0']].
list_to_ser([H|T], D) -->
        !,
        [32],
        term_to_ser_1(H, D),
        list_to_ser(T, D).

args_to_ser([], _) --> 
        !,
        [0')].
args_to_ser([H|T], D) -->
        [32],
        term_to_ser_1(H, D),
        args_to_ser(T, D).
                 
delim(L, L):- L=[C|_], delim_char(C).

delim_char(0')).
delim_char(0']).
delim_char(C):- space_char(C).

prefix([]) --> !.
prefix([A|L]) --> [A], prefix(L).

prefix_atom([]) --> !.
prefix_atom([C|Cs]) -->
        prefix_atom_1(C, Cs).

prefix_atom_1(0'\\, Cs) --> !,
        [0'\\, 0'\\],
        prefix_atom(Cs).
prefix_atom_1(C, Cs) --> 
        {delim_char(C)}, 
        !,
        [0'\\, C],
        prefix_atom(Cs).
prefix_atom_1(C, Cs) -->
        [C],
        prefix_atom(Cs).
           
find_var_number(V, D, I, I):-
        var(D), 
        !,
        D= [V|_].
find_var_number(V, [W|R], I, N):-
        (  V==W
        -> N=I
        ;  J is I+1,
           find_var_number(V, R, J, N)
        ).

% -- Read term -----------------------------------------------------

:- export(ser_to_term/3).
:- export(ser_to_term/4).

ser_to_term(S, T, R):-
        ser_to_term(S, _, T, R).

ser_to_term(S, D, T, R):-
	S= C,
        skip_spaces(C, S1),
        ser_to_term_1(T, D, S1, R).

collect_to_delim(Cs, L, R):-
        (  L=[]
        -> Cs=[], R=L
        ;  L=[C|T],
           (  delim_char(C)
           -> Cs=[],
              R= L
           ;  C= 92, T=[D|M] % 0'\\
           -> Cs=[D|Cs2],
              collect_to_delim(Cs2, M, R)
           ;  Cs=[C|Cs2],
              collect_to_delim(Cs2, T, R)
           )
        ).
           
ser_to_term_1(T, D) -->
        skip_spaces,
        [C],
        ser_to_term_2(C, T, D).

ser_to_term_2(0'$, V, D) --> !,
        collect_to_delim(Cs),
        {string_list(S, Cs)},
        {find_var(S, D, V)}.
ser_to_term_2(0'[, L, D) --> !,
        collect_list(L, D).
ser_to_term_2(0'(, S, D) --> !,
        collect_to_delim(Cs),
        {atom_codes(F, Cs)},
        collect_args(As, D),
        {S =.. [F|As]}.
ser_to_term_2(C, N, _, L, R):-
        collect_to_delim(Cs, [C|L], R),
        (  C>= 0'0, C=<0'9
        -> number_codes(N, Cs)
        ;  atom_codes(N, Cs)
        ).

collect_list(L, D) -->
        skip_spaces,
        (  [0']]
        -> {L=[]}
        ;  {L=[T|R]},
           [C],
           ser_to_term_2(C, T, D),
           collect_list(R, D)
        ).

collect_args(L, D) -->
        skip_spaces,
        (  [0')]
        -> {L=[]}
        ;  {L=[T|R]},
           [C],
           ser_to_term_2(C, T, D),
           collect_args(R, D)
        ).

find_var(S, D, V):-
        (  var(D)
        -> D=[S:V|_]
        ;  D=[S:V|_]
        -> true
        ;  D=[_|R],
           find_var(S, R, V)
        ).

skip_spaces --> [C], {space_char(C)}, !, skip_spaces.
skip_spaces --> [].

space_char(9).
space_char(32).
space_char(10).
space_char(12).
space_char(13).
