:- module(lists, []).

% ----------------
append([], L, L).
append([X|L1], L2, [X|L3]) :-
	append(L1, L2, L3).


% ----------------
member(X, [X|_]).
member(X, [_|L]):-
	member(X, L).

% ----------------
memberchk(X, [X|_]):- !.
memberchk(X, [_|L]):-
	memberchk(X, L).

% ----------------
select(X, [X|L], L).
select(X, [X1|L], [X1|L1]):-
	select(X, L, L1).

% ----------------
reverse(L, R) :-
	reverse(L, [], R).

reverse([], R, R).
reverse([X|L], R0, R) :-
	reverse(L, [X|R0], R).


% ----------------
prefix([], _).
prefix([X|LP], [X|L]) :-
    prefix(LP, L).

% ----------------
count_nth(N, N, [X|_], X).
count_nth(N, NR, [_|L], X):-
	N1 is N + 1,
	count_nth(N1, NR, L, X).

nth(N, L, X):-
	integer(N), !,
	N >= 1,
	count_nth(1, N, L, X), !.
nth(N, L, X):-
	var(N),
	count_nth(1, N, L, X).

% ----------------
count_length([], N, N).
count_length([_|L], N, NR):-
	N1 is N + 1,
	count_length(L, N1, NR).
	
length(L, N):-
	integer(N), !,
	N >= 0,
	count_length(L, 0, N), !.
length(L, N):-
	var(N),
	count_length(L, 0, N).

% ----------------
count_max_list([], Max, Max).
count_max_list([X|L], Max0, Max) :-
	X > Max0, !, 
	count_max_list(L, X, Max).
count_max_list([_|L], Max0, Max) :-
	count_max_list(L, Max0, Max).

max_list([X|L], Max):-
	count_max_list(L, X, Max).
% ----------------
count_min_list([], Min, Min).
count_min_list([X|L], Min0, Min) :-
	X < Min0, !, 
	count_min_list(L, X, Min).
count_min_list([_|L], Min0, Min) :-
	count_min_list(L, Min0, Min).

min_list([X|L], Min):-
	count_min_list(L, X, Min).

% ----------------
count_sum_list([], Sum, Sum).
count_sum_list([X|L], Sum0, Sum) :-
	Sum1 is Sum0 + X,
	count_sum_list(L, Sum1, Sum).

sum_list(L, Sum):-
	count_sum_list(L, 0, Sum).

% ----------------
last([X], X).
last([_|L], X) :-
	last(L, X).

% ----------------
list_or_partial_list(X) :-
	var(X), !.
list_or_partial_list([]).
list_or_partial_list([_|L]) :-
	list_or_partial_list(L).

