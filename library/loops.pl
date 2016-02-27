:- module(loops, [repeat/1, between/3],[assertions]).

repeat(N) :- N > 0.
repeat(N) :- N > 0, N1 is N-1, repeat(N1).

between(N, N, M) :- N =< M.
between(I, N, M) :- N < M, N1 is N+1, between(I, N1, M).

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
