my_format(F, A) :-
	format_i(F, A).

my_format(S, F, A) :-
	current_output(CS),
	set_output(S),
	(   catch(format_i(F, A), E, ((set_output(CS), throw(E)))), !,
	    set_output(CS)
        ;   set_output(CS), fail
        ).

format_i(Format, Args) :-
	atom(Format), !,
	atom_codes(Format, L),
	format_string(L, Args).
format_i(Format, Args) :-
	format_string(Format, Args).

format_string([], _).
format_string([0'~|L], Args) :- !,
	format_escape(L, Args).
format_string([C|L], Args) :-
	put_code(C),
	format_string(L, Args).

format_escape(L0, Args0) :-
	get_num(L0, Args0, N, [C|L1], Args1),
	print_arg(C, N, Args1, Args2),
	format_string(L1, Args2).

print_arg(0'a, N, [Atom|Args], Args) :- !,
	atom(Atom),
	N = [],
	write(Atom).
print_arg(0'c, N, [Char|Args], Args) :- !,
	integer(Char),
	(   N = [] -> N1 = 1
	;   N >= 0 -> N1 = N
	),
	n_put_code(N1, Char).
print_arg(0'd, N, [Int|Args], Args) :- !,
	integer(Int),
	(   (N = []; N = 0) -> write(Int)
	;   N > 0 -> n_put_decimal(N, Int)
	).
print_arg(0'p, N, [Term|Args], Args) :- !,
	N = [],
	write(Term).
print_arg(0'q, N, [Term|Args], Args) :- !,
	N = [],
	writeq(Term).
print_arg(0'w, N, [Term|Args], Args) :- !,
	N = [],
	write(Term).
print_arg(0'~, N, Args, Args) :- !,
	N = [],
	put_code(0'~).
print_arg(0'n, N, Args, Args) :- !,
	(   N = [] -> N1 = 1
	;   N >= 0 -> N1 = N
	),
% vvv %%%% pts %%%% not(?) for SWI-Prolog
%	n_put_code(N1, 0'\n). % 0'\n == 10
	n_put_code(N1, 10). % 0'\n == 10
print_arg(0'N, _N, Args, Args) :- !,
% vvv %%%% pts %%%% not(?) for SWI-Prolog
%	put_code(0'\n).
	put_code(10).
print_arg(0'@, N, [Call|Args], Args) :- !,
	N = [],
	call(Call).
print_arg(X, N, Args, Args) :-
	put_code(0'~),
	( number(N) -> write(N) ; true ),
	put_code(X).

n_put_decimal(N, Int) :-
	number_codes(Int, L),
	length(L, LN),
	(   LN =< N ->
	    NZ is N - LN,
	    put_code(0'0), put_code(0'.), n_put_code(NZ, 0'0), put_string(L)
	;   length(L2, N),
	    append(L1, L2, L),
	    put_string(L1), put_code(0'.), put_string(L2)
	).
	
n_put_code(0, _C) :- !.
n_put_code(N, C) :-
	put_code(C),
	N1 is N - 1,
	n_put_code(N1, C).

put_string([]).
put_string([X|L]) :-
	put_code(X),
	put_string(L).

get_num([*|L], [N|Args], N, L, Args) :- !,
	integer(N).
get_num([D|L0], Args, N, L, Args) :-
	is_digit(D), !,
	get_num_digits(D, L0, 0, N, L).
get_num(L, Args, [], L, Args).

get_num_digits(D, [X|L0], N0, N, L) :-
	is_digit(D), !,
	N1 is N0 * 10 + (D - 0'0),
	get_num_digits(X, L0, N1, N, L).
get_num_digits(X, L, N, N, [X|L]).

is_digit(D) :-
	D >= 0'0, D =< 0'9.

% ----------------
