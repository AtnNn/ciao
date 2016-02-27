:- module(loops, [repeat/1],[assertions]).

repeat(N) :- N > 0.
repeat(N) :- N > 0, N1 is N-1, repeat(N1).

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
