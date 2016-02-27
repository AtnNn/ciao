:- module(_example7,[average_initial/2,average/3,average_final/3,tall/2,fast/2,good_player/2],ciaopp).

:- op(700,xfx,[.=.,.<>.,.<.,.=<.,.>.,.>=.]).

:- include(library('clpr/ops')).

:- use_module(library('clpr/clprt')).

:- use_module(library('clpr/nl_eval'),[solve_abs/2,solve_mix/4,solve_mult/3,solve_pow/3,solve_trig/3]).

:- include(library('clpr/clpr')).

:- op(1200,xfx,:~).

:- op(1200,xf,:~).

:- op(1200,xfx,:=).

:- op(1200,xf,:=).

:- op(1200,xfx,:#).

:- op(1175,fx,=>).

:- op(1150,fx,fnot).

:- op(1150,fx,aggr).

:- op(1150,fx,fuzzy).

:- op(1190,fx,fuzzy_predicate).

:- op(1190,fx,fuzzy_discrete).

:- include(library('fuzzy/ops')).

:- use_module(library('fuzzy/faggr')).

:- op(1190,fx,min).

:- op(1190,fx,luka).

:- op(1190,fx,prod).

:- op(1190,fx,max).

:- op(1190,fx,dluka).

:- op(1190,fx,dprod).

:- new_declaration(is_fuzzy/3,on).

:- op(1190,fx,average).

average_initial(ListInitial,ListInitial).

average(X,Y,Z) :-
        solve_generic_3(eq,-0.0,Y,-1,Z,1,X,-1).

average_final(ListTemp,ValorTemp,ValorFinal) :-
        solve_generic_2(eq,0.0,ValorTemp,-1,ValorFinal,2.0).

tall(john,0.8) :-
        true,
        true.

fast(john,0.7) :-
        true,
        true.

good_player(_1,_2) :-
        tall(_1,_3),
        fast(_1,_4),
        inject([_3,_4],average,_2),
        solve_generic_1(le,0,_2,-1),
        solve_generic_1(le,-1,_2,1).

:- is_fuzzy(tall,2,truth).

:- is_fuzzy(fast,2,truth).

:- is_fuzzy(good_player,2,truth).

