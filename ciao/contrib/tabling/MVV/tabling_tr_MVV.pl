:- module(tabling_tr_MVV,
        [
            do_term_expansion/3
        ]).

:- use_module(library(lists), 
	[
	    reverse/2,
	    append/3
	]).

:- dynamic 'trans$tabled'/2, 'trans$default'/1.

:- dynamic 'trans$bridge'/1.
:- dynamic 'trans$prolog'/1, 'trans$tab'/2.
:- dynamic trans_expanding/0, 'trans$cont'/1.

:- dynamic module_name/1.

do_term_expansion(0,_,Module) :-
	assert('trans$tabled'(0,0)), retractall('trans$tabled'(_,_)),
	assert('trans$default'((prolog))),
	retractall('trans$cont'(_)), assert('trans$cont'(0)),
	(
	    Module = user(_) ->
	    Mod = "user"
	;
	    atom_codes(Module,Mod)
	),
	assert(module_name(Mod)).

do_term_expansion(end_of_file,_,_) :- !,
	retractall('trans$default'(_)),
	assert('trans$default'((prolog))),
	retractall('trans$prolog'(_)),
	retractall('trans$tab'(_,_)),
	retractall('trans$cont'(_)),
	retractall(module_name(_)),
	assert('trans$cont'(0)),
	fail.

do_term_expansion(':-'(Com),Clauses,_) :- !,
       	expand_command(Com,Clauses).
 %%       	expand_command(Com,Clauses),
 %%       	display(Clauses), nl.

do_term_expansion(Clause,Clauses,_) :-
	( 
	    Clause = (Head :- Body) -> 
	    true
	; 
	    Head = Clause, 
	    Body = true 
	),
	functor(Head,P,A),
	Pred = P/A,
	( 
	    'trans$tab'(P,A) ->
	    convert_tabled_clause(Head,Body,Clauses)
        ; 
	    'trans$prolog'(Pred) ->
	    Clauses = Clause
        ; 
	    'trans$default'(Default),
	    ( 
		Default == (prolog) ->
		Clauses = Clause
	    ; 
		display('EEEEEHHHHH!!! ENTRA EN DEFAULT'), nl
	    )
       	).
 %%       	),
 %%       	display(Clauses), nl.

expand_command(table(Preds),Clauses) :-
	!, expand_command_table(Preds,Clauses,[]).
 %% expand_command(Cl,Cl).

expand_command_table((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_table_one(Pred,Clauses0,Clauses1),
	expand_command_table(Preds,Clauses1,Clauses).
expand_command_table(Pred,Clauses0,Clauses) :-
	expand_command_table_one(Pred,Clauses0,Clauses).

expand_command_table_one(Pspec,Clauses0,Clauses) :-
	  ( 
	      Pspec = P/A -> true
	  ; 
	      P = Pspec, A = 0 
	  ),
	  functor(H,P,A),
	  ( 
	      'trans$tab'(P,A) ->
	      Clauses0 = Clauses
	  ; 
	      assert('trans$tab'(P,A)),
	      retractall('trans$tabled'(P,A)),
	      assert('trans$tabled'(P,A)),
	      get_pred_init(H,PredInit),
	      Clauses0 = [(H :- tabled_call(PredInit)
			  )|Clauses]
	  ).

convert_tabled_clause(Head, Body, (NewHead :- NBody)) :-
	new_trans_head(Head, NewHead),      %it gets the new head of the tabled clauses
	conj_to_list(Body, Blist),          %it tranforms the body into a list
	convert_tabled_body(Blist, NBlist),
	list_to_conj(NBlist, NBody).        %GConj is Guard as a set

new_trans_head(Head,NewHead) :-
	functor(Head,P,Arity),
	name(P,Pl),
        append(Pl,"0",MName),
	name(Npred,MName),
	functor(NewHead,Npred,Arity),
        put_args(NewHead,Head,Arity).

put_args(_,_,0) :- !.
put_args(R,O,1) :- !, 
	arg(1,O,Arg), 
	arg(1,R,Arg).
put_args(R,O,N) :- !, 
	arg(N,O,Arg), 
	arg(N,R,Arg),
	N1 is N - 1,
	put_args(R,O,N1).


convert_tabled_body([],[new_answer]) :- !.

convert_tabled_body([HBody|RBody], [NHBody|NRBody]) :-
	convert_tabled_term(HBody,NHBody),
	convert_tabled_body(RBody,NRBody).

convert_tabled_term(T,NT) :-
	functor(T,NameT,ArityT),
	( 
	    'trans$tab'(NameT,ArityT) ->
	    get_pred_init(T,AuxT),
	    NT = tabled_call(AuxT)
	  ;
	    NT = T
	).
	
get_pred_init(Call,ContPred) :-
	functor(Call,F,Arity),
        N1 is 0,
	name(N1,NName),
	name(F,FName),	
	module_name(Module),
	append(Module,":",MAux),
	append(MAux,FName,FullName),
	append(FullName,NName,Name),
	name(ContPredName,Name),
	functor(ContPred,ContPredName,Arity),
 	put_args(ContPred,Call,Arity).

conj_to_list(Term,List) :-
	conj_to_list_3(Term,List,[]).
conj_to_list_3(Term,List0,List) :-
	( Term = (T1,T2) ->
	  conj_to_list_3(T1,List0,List1),
	  conj_to_list_3(T2,List1,List)
        ; Term == true ->
	  List0 = List
        ; List0 = [Term|List]
        ).

list_to_conj([],true).
list_to_conj([Lit|List],G0) :-	
	( List == [] ->
	  G0 = Lit
        ; G0 = (Lit,G),
	  list_to_conj(List,G)
        ).

trans_built_in(tabled_call(_,_)).
trans_built_in(new_answer(_,_)).
trans_built_in(consume_answer(_,_)).


