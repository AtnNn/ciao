:- use_module(library('remote/ciao_server_rt'), [serve/0]).
:- include(library(hlc)).

%% :- use_module(library(prolog_sys), [predicate_property/2]).


 %% call_in_user(G):- 
 %%         (
 %%             predicate_property(G, _) ->
 %%             call(G)
 %%         ;
 %%             display(user_error, 'Error, predicate not available!!!!'),
 %%             nl(user_error)
 %%         ).

call_in_user(G) :- call(G).

main:- serve.
