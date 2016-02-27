:- include(library('remote/ciao_server')).


:- use_module(queens, [queens/2]). 

:- use_module(simpleclp, [c/2]).

:- use_module(queensclp, [queensclp/2, constrain_values/3, place_queens/2]).

%% Just a fact.  This is also exported.
p(a).

