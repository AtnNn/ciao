:- module(between, [between/3], []).

between(N, Min, Max) :- integer(N), !, N >= Min, N =<  Max.
between(V, Min, Max) :- var(V), Min =< Max, between_nd(V, Min, Max).

between_nd(Min, Min, _).
between_nd(N, Min, Max) :-
        Min < Max, NMin is Min+1,
        between_nd(N, NMin, Max).
