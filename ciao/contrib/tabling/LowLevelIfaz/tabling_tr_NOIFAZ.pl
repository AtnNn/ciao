:- module(tabling_tr_NOIFAZ,
        [
            do_term_expansion/3
        ]).

:- use_module(library(lists), 
	[
	    reverse/2,
	    append/3
	]).

:- dynamic 'trans$tabled'/2, 'trans$default'/1.

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
%	display(Clauses), nl.

do_term_expansion(Clause,Clauses,_) :-
	( Clause = (Head :- Body) -> true; Head = Clause, Body = true ),
	functor(Head,P,A),
	Pred = P/A,
	( 'trans$tab'(P,A) ->
	  convert_tabled_clause(Head,Body,Clauses)
        ; 'trans$prolog'(Pred) ->
	  Clauses = Clause
        ; 'trans$default'(Default),
	  ( Default == (prolog) ->
	    Clauses = Clause
	  ; ( 'trans$tab'(P,A) ->
	      convert_tabled_clause(Head,Body,Clauses)
	    ; assert('trans$tab'(P,A)),
	      retractall('trans$tabled'(P,A)),
	      assert('trans$tabled'(P,A)),
              get_pred_init(Head,PredInit),
	      Clauses = [(Head :- tabled_call(Head,Id,[],PredInit,'true'),consume_answer(Head,Id,_))|_Cls],
              convert_tabled_clause(Head,Body,_RestClauses)
	    )
	  )
        ).
	%display('SALIDA'), nl,
	%display(Clauses), nl,
	%display('Fin SALIDA'), nl.


expand_command(table(Preds),[Clauses0|Clauses]) :-
	add_discontiguous(Preds, [], Clauses0), 
	expand_command_table(Preds,Clauses,[]).
expand_command(prolog(Preds),Clauses) :-
	expand_command_prolog(Preds,Clauses,[]).
expand_command(multifile(Preds),(:-multifile(NewPreds))) :-
	add_table_preds(Preds,NewPreds,[]).
expand_command(dynamic(Preds),(:-dynamic(NewPreds))) :-
	add_table_preds(Preds,NewPreds,[]),
	telling(T),
	tell('dynamic'),
	portray_clause(:-(p(_3501),=(_3501,3))),
	tell(T).

expand_command(default(D),[]) :-
	( (D == (prolog); D == (tabled)) ->
	  retractall('trans$default'(_)),
	  assert('trans$default'(D))
        ; write('Warning: illegal default '),
	  write(D),
	  write(' ignored.'),
	  nl
        ).

add_discontiguous(','(P,PList), Clauses0, Clauses1) :- 
	transform(P,PAux),
	add_discontiguous(PList,[':-'(discontiguous(PAux))|Clauses0], Clauses1).

add_discontiguous(P, _, ':-'(discontiguous(PAux))) :-
	transform(P,PAux).

transform(P,PAux) :-
	functor(P,F,Args),
	arg(1,P,Name),
	arg(2,P,Arity),
	atom_concat(Name,'0',NName),
	functor(PAux,F,Args),
	arg(1,PAux,NName),
	arg(2,PAux,Arity).

expand_command_table((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_table_one(Pred,Clauses0,Clauses1),
	expand_command_table(Preds,Clauses1,Clauses).
expand_command_table(Pred,Clauses0,Clauses) :-
	expand_command_table_one(Pred,Clauses0,Clauses).

expand_command_table_one(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Pred = P/A,
	  functor(H,P,A),
	  ( ( predicate_property(H,built_in); trans_built_in(H) ) ->
	    write('ERROR: Cannot table built_in '),
	    write(Pred), nl,
	    Clauses0 = Clauses
	  ; 'trans$prolog'(Pred) ->
	    write('ERROR: '),
	    write(Pred),
	    write(' assumed to be a Prolog predicate'),
	    nl,
	    tab(7),
	    write('But later declared a tabled predicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; 'trans$tab'(P,A) ->
	    Clauses0 = Clauses
	  ; assert('trans$tab'(P,A)),
	    retractall('trans$tabled'(P,A)),
	    assert('trans$tabled'(P,A)),
	    get_pred_init(H,PredInit),
	    Clauses0 = [
            (H :- tabled_call(H,Id,[],PredInit,'true'),consume_answer(H,Id,_))|Clauses]
	  ).

expand_command_prolog((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_prolog_one(Pred,Clauses0,Clauses1),
	expand_command_prolog(Preds,Clauses1,Clauses).
expand_command_prolog(Pred,Clauses0,Clauses) :-
	expand_command_prolog_one(Pred,Clauses0,Clauses).

expand_command_prolog_one(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Pred = P/A,
	  ( 'trans$tab'(P,A) ->
	    write('ERROR: '),
	    write(Pred),
	    write(' assumed to be a tabled predicate'),
	    nl,
	    tab(7),
	    write('But later declared a Prolog predicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; retractall('trans$tab'(P,A)),
	    retractall('trans$tabled'(P,A)),
%	    ( 'trans$prolog'(Pred) ->
%	      true
%	    ; assert('trans$prolog'(Pred))
%	    ),
	    Clauses0 = [(:- retractall('trans$tabled'(P,A)))|Clauses]
          ).

add_table_preds(Preds,NewPreds0,NewPreds) :-
	( Preds == [] ->
	  NewPreds0 = NewPreds
        ; Preds = [P|Ps] ->
	  add_table_preds(P,NewPreds0,NewPreds1),
	  add_table_preds(Ps,NewPreds1,NewPreds)
        ; Preds = (P,Ps) ->
	  add_table_preds(P,NewPreds0,NewPreds1),
	  add_table_preds(Ps,NewPreds1,NewPreds)
        ; ( Preds = P/A -> true; P = Preds, A = 0 ),
	  ( 'trans$tab'(P,A) ->
	    name(P,Pl),
	    name(NewP,[115,108,103,36|Pl]), % 'trans$'
	    NewA is A+1,
	    NewPreds0 = [P/A,NewP/NewA|NewPreds]
	  ; NewPreds0 = [P/A|NewPreds]
          )
        ).

convert_tabled_clause(Head,Body,Clauses0) :-
	new_trans_head(Head,Cid,NewHead),
	Ans = new_answer(Head,Cid),
	%nl, display('Ans - '), display(Ans), nl,
	vars_in_term(Head,HeadVars),
	%nl, display('HeadVars - '), display(HeadVars), nl,
	%nl, display('Ans - '), display(Ans), nl,
	conj_to_list(Body,Blist),
	%nl, display('BList - '), display(Blist), nl,
	extract_guard(Blist,Guard,[],NBlist,Clauses0,Clauses1),
	%nl, display('Guard - '), display(Guard), nl,
	%nl, display('NBlist - '), display(NBlist), nl,
	%nl, display('Clauses0 - '), display(Clauses0), nl,
	%nl, display('Clauses1 - '), display(Clauses1), nl,
	list_to_conj(Guard,Gconj),
	vars_in_term(Gconj,GconjVars),
	%nl, display('Gconj - '), display(Gconj), nl,
	%nl, display('GconjVars - '), display(GconjVars), nl,
	convert_tabled_body(NBlist,NewHead,0,Gconj,Ans,HeadVars,Cid,Clauses1,GconjVars,_ListAux).

convert_tabled_body([],NewHead,_Delay,Gconj,Ans,_HeadVars,_Cid,Clauses,_,_) :- !,

	( Gconj == true ->
	  Clauses = [(NewHead :- Ans)]
	; Clauses = [(NewHead :- Gconj,Ans)]
	).
convert_tabled_body(Blist,NewHead,Delay,Gconj,Ans,HeadVars,Cid,Clauses0,ListVars,ListAux) :-
	%nl, display('ListVars antes - '), display(ListVars), nl,
	get_transcall(Blist,HeadVars,Delay,Cid,TRANSCall,Cont,Delay1,ListVars,ListAux),
	%nl, display('Blist - '), display(Blist), nl,
	%nl, display('Delay - '), display(Delay), nl,
	%nl, display('TRANSCall - '), display(TRANSCall), nl,
	%nl, display('Cont - '), display(Cont), nl,
	%nl, display('Delay1 - '), display(Delay1), nl,
	%nl, display('ListVars - '), display(ListVars), nl,
	%nl, display('ListAux - '), display(ListAux), nl,

	( Gconj == true ->
	  Clauses0 = [(NewHead :- TRANSCall)|Clauses1]
	; Clauses0 = [(NewHead :- Gconj,TRANSCall)|Clauses1]
	),
	Blist = [_|Rest],
	extract_guard(Rest,NGuard,[],NBlist,Clauses1,Clauses2),
	list_to_conj(NGuard,NGconj),
	convert_tabled_body(NBlist,Cont,Delay1,NGconj,Ans,HeadVars,Cid,Clauses2,ListAux,_ListAux2).

get_transcall(Blist,HeadVars,_Delay,Cid,TRANSCall,Cont,_Delay1,ListVars,Diff) :-
	%nl, display('Blist - '), display(Blist), nl,
	%nl, display('HeadVars - '), display(HeadVars), nl,
	%nl, display('TRANSCall - '), display(TRANSCall), nl,
	%nl, display('Cont - '), display(Cont), nl,
	%nl, display('ListVars - '), display(ListVars), nl,
	%nl, display('Diff - '), display(Diff), nl,
	Blist = [Lit|Rest],
	Lit = Call,
	%nl, display('CALL - '), display(Call), nl,
        vars_in_term(Blist,_BodyVars),
	TRANSCall = tabled_call(Call,Cid,Diff,PredInit,ContPredMod),
	get_cont_pred(Call,ContPred,ContPredMod),
	get_pred_init(Call,PredInit),
	functor(Cont,ContPred,3),
	arg(1,Cont,Call),

	%nl, display('Diff en MEDIO - '), display(Diff), nl,
        ('trans$tab'(_P,A) ->
        getArgs(Call,1,A,ListArgs),
	%nl, display('Call - '), display(Call), nl,
	%nl, display('A - '), display(A), nl,
	%nl, display('ListArgs - '), display(ListArgs), nl,
	lists:reverse(HeadVars,HeadVars1),
	append(HeadVars1,ListArgs,NewList),
	%nl, display('NewList - '), display(NewList), nl,
	repeat(NewList,NewList1),
	%nl, display('NewList1 - '), display(NewList1), nl,
	lists:reverse(NewList1,NewList2),
	vars_in_term(Call,CallVars),
	%nl, display('CallVars - '), display(CallVars), nl,
	append(CallVars,ListVars,LVarsAux),
	repeat(LVarsAux,LVarsFinal),
	%nl, display('LVarsFinal - '), display(LVarsFinal), nl,
	vars_in_term(Rest,RestVars),
	%nl, display('RestVars - '), display(RestVars), nl,
	intersection(LVarsFinal,RestVars,MoreVars),
	%nl, display('MoreVars - '), display(MoreVars), nl,
        append(MoreVars,NewList2,ListFinal),
	%nl, display('ListFinal - '), display(ListFinal), nl,
	repeat(ListFinal,ListFinal1),
	%nl, display('ListFinal1 - '), display(ListFinal1), nl,
	lists:reverse(ListFinal1,Diff)
        ;false),
	arg(2,Cont,Cid),
	arg(3,Cont,Diff).
	%nl, display('Blist OUT- '), display(Blist), nl,
	%nl, display('HeadVars OUT- '), display(HeadVars), nl,
	%nl, display('TRANSCall OUT- '), display(TRANSCall), nl,
	%nl, display('Cont OUT- '), display(Cont), nl,
	%nl, display('ListVars OUT- '), display(ListVars), nl,
	%nl, display('Diff OUT- '), display(Diff), nl.

getArgs(Call,A,A,[ARG]):-arg(A,Call,ARG).
getArgs(Call,I,A,[ARG1|T]):-arg(I,Call,ARG1),I1 is I+1,getArgs(Call,I1,A,T).

intersection([],_,[]).
intersection([H|R],L,[H|Raux]) :-
	membervar(H,L),
	!, intersection(R,L,Raux).
intersection([_|R],L,Raux) :-
	intersection(R,L,Raux).


repeat(L,L1):-repeat_3(L,[],L1).

repeat_3([],R,R).
repeat_3([H|T],R,R1):-membervar(H,R),repeat_3(T,R,R1).
repeat_3([H|T],R,R1):-repeat_3(T,[H|R],R1).


reverse([],[]).
reverse([H|T],L):- lists:reverse(T,R1), append(R1,[H],L).



vars_in_term(Term,Vars) :-
	get_vars_in_term(Term,[],Vars).

get_vars_in_term(Term,Vars0,Vars) :-
	( var(Term) ->
	  ( membervar(Term,Vars0) ->
	    Vars = Vars0
	  ; Vars = [Term|Vars0]
	  )
	; functor(Term,_,N),
	  get_vars_in_args(0,N,Term,Vars0,Vars)
	).

get_vars_in_args(N,N,_,Vars,Vars).
get_vars_in_args(N0,N,Term,Vars0,Vars) :-
	N0 < N,
	N1 is N0+1,
	arg(N1,Term,Arg),
	get_vars_in_term(Arg,Vars0,Vars1),
	get_vars_in_args(N1,N,Term,Vars1,Vars).

difference([],_,Vars,Vars).
difference([X|Xs],Ys,Vars0,Vars) :-
	( membervar(X,Ys) ->
	  difference(Xs,Ys,Vars0,Vars)
	; difference(Xs,Ys,[X|Vars0],Vars)
	).

membervar(Var,[V|_]) :- 
	Var == V,
	!.
membervar(Var,[_|Vars]) :-
	membervar(Var,Vars).

get_cont_pred(Call,ContPred,ContPredMod) :-
	functor(Call,F,_),
	retract('trans$cont'(N)),
	N1 is N+1,
	assert('trans$cont'(N1)),
	name(N1,NName),
	name(F,FName),	
%        append(FName,".",MName),
	module_name(Mod),
	append(Mod,":",MAux),
	append(FName,NName,Name),
	append(MAux,Name,ModName),
        %append(FName,"",MName),
	name(ContPred,Name),
	name(ContPredMod,ModName).

get_pred_init(Call,ContPred) :-
	functor(Call,F,_),
        N1 is 0,
	name(N1,NName),
	name(F,FName),	
%        append(FName,".",MName),
	module_name(Module),
	append(Module,":",MAux),
	append(MAux,FName,FullName),
	%append(FullName,"",MName),
	append(FullName,NName,Name),
	name(ContPred,Name).

convert_tabled_body_6(_HeadVars,_Cid,Ans,CHead,Blist,Clauses0) :-
	Blist = [_|Rest],
	extract_guard(Rest,Guard,[],NBlist,Clauses0,Clauses1),
	list_to_conj(Guard,Gconj),
	( NBlist == [] ->
	  ( Gconj == true ->
	    Clauses1 = [(CHead :- Ans)]
	  ; Clauses1 = [(CHead :- Gconj,Ans)]
	  )
	; true
	).

convert_univ_clause(Head,Body,Clauses) :-
	nl,
	write( 'julie ** call convert_univ_clause '),
	nl,
	disj_to_list(Body,Blist),
	new_trans_head(Head,all(Blist),NewHead),
	write( 'julie ** in convert_univ_clause NewHead ='),
	nl,
	write(NewHead),
	nl,
	
	Clauses = [(NewHead :- ( ground(Head) -> 
	                         true
			       ; write('Error: Non-ground call '),
			         write(Head),
				 write(' in a clause with universal disjunction.'),
				 nl
			       ))].

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

disj_to_list(Term,List) :-
	disj_to_list_3(Term,List,[]).

disj_to_list_3(Term,List0,List) :-
	( Term = (T1;T2) ->
	  disj_to_list_3(T1,List0,List1),
	  disj_to_list_3(T2,List1,List)
        ; Term == true ->
	  List0 = List
        ; List0 = [Term|List]
        ).

extract_guard([],G,G,[],Cls,Cls).
extract_guard([Lit|List],G0,G,Rest,Cls0,Cls) :-
	( Lit = (\+N) ->
	  Nlit = N
        ; Nlit = Lit
        ),
	( ( predicate_property(Nlit,built_in); trans_built_in(Nlit) ) ->
	  G0 = [Lit|G1],
	  extract_guard(List,G1,G,Rest,Cls0,Cls)
        ; functor(Nlit,P,A),
	  Pred = P/A,
	  ( 'trans$tab'(P,A) ->
	    G0 = G,
	    Rest = [Lit|List],
	    Cls0 = Cls
	  ; 'trans$prolog'(Pred) ->
	    G0 = [Lit|G1],
	    extract_guard(List,G1,G,Rest,Cls0,Cls)
	  ; 'trans$default'((prolog)) ->
	    G0 = [Lit|G1],
	    assert('trans$prolog'(Pred)),
%	    Cls0 = [(:- 'trans$prolog'(Pred) -> true; assert('trans$prolog'(Pred)))|Cls1],
	    extract_guard(List,G1,G,Rest,Cls1,Cls),
	    Cls0 = Cls1

	  ; 'trans$default'((tabled)) ->
	    G0 = G,
	    Rest = [Lit|List],
	    assert('trans$tab'(P,A)),
	    retractall('trans$tabled'(P,A)),
            assert('trans$tabled'(P,A)),
	    functor(Head,P,A),
            get_pred_init(Head,PredInit),             
	    Cls0 = [(:- retractall('trans$tabled'(P,A)), assert('trans$tabled'(P,A))),
                    (Head :- tabled_call(Head,Id,[],PredInit,'true'),consume_answer(Head,Id,_))|Cls]
	  )
        ).

list_to_conj([],true).
list_to_conj([Lit|List],G0) :-	( List == [] ->
	  G0 = Lit
        ; G0 = (Lit,G),
	  list_to_conj(List,G)
        ).

new_trans_head(Head,Body,NewHead) :-
	functor(Head,P,_A),
	name(P,Pl),

%        append(Pl,".0",MName),
        append(Pl,"0",MName),
	name(Npred,MName),

	Narity is 2,
	functor(NewHead,Npred,Narity),
        arg(1,NewHead,Head),
        arg(2,NewHead,Body).



put_in_args(A,A,_,_).
put_in_args(A0,A,Head,NewHead) :-
	A0 < A,
	A1 is A0+1,
	arg(A1,Head,Arg),
	arg(A1,NewHead,Arg),
	put_in_args(A1,A,Head,NewHead).

trans_built_in(tabled_call(_,_)).
trans_built_in(new_answer(_,_)).
trans_built_in(consume_answer(_,_)).


