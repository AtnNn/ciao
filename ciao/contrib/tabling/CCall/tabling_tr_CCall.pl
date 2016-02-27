:- module(tabling_tr_CCall,
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
 %%     	expand_command(Com,Clauses),
 %%     	display(Clauses), nl.

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
	    'trans$bridge'(Pred) ->
	    convert_bridge_clause(Head,Body,Clauses)
        ; 
	    'trans$default'(Default),
	    ( 
		Default == (prolog) ->
		Clauses = Clause
	    ; 
		display('EEEEEHHHHH!!! ENTRA EN DEFAULT'), nl
	    )
     	).
 %%     	),
 %%     	display(Clauses), nl.

expand_command(table(Preds),Cl) :-
	add_discontiguous(Preds, [], Clauses0), 
	expand_command_table(Preds,Clauses,[]),
	append(Clauses0, Clauses, Cl).
expand_command(bridge(Preds),Cl) :-
	add_discontiguous_bridge(Preds, [], Clauses0), 
	expand_command_bridge(Preds,Clauses,[]),
	append(Clauses0, Clauses, Cl).

add_discontiguous(','(P,PList), Clauses0, Clauses1) :- 
	transform(P,2,PAux),
	add_discontiguous(PList,[':-'(discontiguous(PAux))|Clauses0], Clauses1).
add_discontiguous(P, Clauses0, [':-'(discontiguous(PAux))|Clauses0]) :-
	transform(P,2,PAux).

add_discontiguous_bridge(','(P,PList), Clauses0, Clauses1) :- 
	transform(P,3,PAux),
	add_discontiguous_bridge(PList,[':-'(discontiguous(P)),':-'(discontiguous(PAux))|Clauses0], Clauses1).
add_discontiguous_bridge(P, Clauses0, [':-'(discontiguous(P)), ':-'(discontiguous(PAux))|Clauses0]) :-
	transform(P,3,PAux).

transform(P,Arity,PAux) :-
	functor(P,F,Args),
	arg(1,P,Name),
	atom_concat(Name,'0',NName),
	functor(PAux,F,Args),
	arg(1,PAux,NName),
	arg(2,PAux,Arity).

expand_command_table((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_table_one(Pred,Clauses0,Clauses1),
	expand_command_table(Preds,Clauses1,Clauses).
expand_command_table(Pred,Clauses0,Clauses) :-
	expand_command_table_one(Pred,Clauses0,Clauses).

expand_command_bridge((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_bridge_one(Pred,Clauses0,Clauses1),
	expand_command_bridge(Preds,Clauses1,Clauses).
expand_command_bridge(Pred,Clauses0,Clauses) :-
	expand_command_bridge_one(Pred,Clauses0,Clauses).

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
	      Clauses0 = [(H :- 
			  tabled_call(H, Sid, PredInit, PT, _),
			  (
			      '$meta_call'(PT) ->
			      consume_answer(H,Sid)
			  ;
			      resume_ccalls(Sid,CCall,0,0,0),
			      '$meta_call'(CCall),
			      consume_answer(H,Sid)
			  )
			  )|Clauses]
	  ).

expand_command_bridge_one(Pspec,Clauses0,Clauses) :-
	  ( 
	      Pspec = P/A -> true
	  ; 
	      P = Pspec, A = 0 
	  ),
	  functor(_H,P,A),
	  ( 
	      'trans$bridge'(P/A) ->
	      Clauses0 = Clauses
	  ; 
	      assert('trans$bridge'(P/A)),
	      Clauses0 = Clauses
	  ).

convert_tabled_clause(Head,Body,Clauses0) :-
	new_trans_head(Head,Cid,NewHead), %it gets the new head of the tabled clauses
        % if bridge meter el call, sino es answer
	Ans = (new_answer(Head,Cid)),     %Ans stores the last answer primitive
	vars_in_term(Head,HeadVars),      %it gets vars of the original head
	conj_to_list(Body,Blist),         %it tranforms the body into a list
	extract_guard(Blist,Guard,[],NBlist,Clauses0,Clauses1), %Guard = code until bridge or tabled predicate
	list_to_conj(Guard,Gconj),        %GConj is Guard as a set
	vars_in_term(Gconj,GconjVars),    %It gets vars of the guard
	convert_tabled_body(NBlist,NewHead,0,Gconj,Ans,HeadVars,Cid,Clauses1,GconjVars,_ListAux).

new_trans_head(Head,Body,NewHead) :-
	functor(Head,P,_A),
	name(P,Pl),
        append(Pl,"0",MName),
	name(Npred,MName),
	Narity is 2,
	functor(NewHead,Npred,Narity),
        arg(1,NewHead,Head),
        arg(2,NewHead,Body).

convert_bridge_clause(Head,Body,Clauses) :-
	new_trans_head_bridge(Head,Cid,NewHead), %it gets the new head of the tabled clauses
	vars_in_term(Head,HeadVars),      %it gets vars of the original head
	conj_to_list(Body,Blist),         %it tranforms the body into a list
	extract_guard(Blist,Guard,[],NBlist,Clauses0,Clauses1), %Guard = code until bridge or tabled predicate
	list_to_conj(Guard,Gconj),        %GConj is Guard as a set
	vars_in_term(Gconj,GconjVars),    %It gets vars of the guard
	convert_bridged_body(NBlist,NewHead,0,Gconj,NewHead,HeadVars,Cid,Clauses1,GconjVars,_ListAux),
	Clauses = [(Head :- Body)|Clauses0].

new_trans_head_bridge(Head,Body,NewHead) :-
	functor(Head,P,_A),
	name(P,Pl),
        append(Pl,"0",MName),
	name(Npred,MName),
	Narity is 3,
	functor(NewHead,Npred,Narity),
        arg(1,NewHead,Head),
        arg(2,NewHead,Body).

convert_tabled_body([],NewHead,_Delay,Gconj,Ans,_HeadVars,_Cid,Clauses,_,_) :- !,
	( Gconj == true ->
	  Clauses = [(NewHead :- Ans)]    %like bridge, but Ans is a meta_call
	; Clauses = [(NewHead :- Gconj,Ans)]
	).

convert_tabled_body(Blist,NewHead,Delay,Gconj,Ans,HeadVars,Cid,Clauses0,ListVars,ListAux) :-
	get_transcall(Blist,HeadVars,Delay,Cid,TRANSCall,Cont,Delay1,ListVars,ListAux,NewHead),
	( Gconj == true ->
	  Clauses0 = [(NewHead :- TRANSCall)|Clauses1]
	; Clauses0 = [(NewHead :- Gconj,TRANSCall)|Clauses1]
	),
	Blist = [_|Rest],
	extract_guard(Rest,NGuard,[],NBlist,Clauses1,Clauses2),
	list_to_conj(NGuard,NGconj),
	convert_tabled_body(NBlist,Cont,Delay1,NGconj,Ans,HeadVars,Cid,Clauses2,ListAux,_ListAux2).

convert_bridged_body([],NewHead,_Delay,Gconj,_Ans,_HeadVars,_Cid,Clauses,_,_) :- !,
	arg(4,NewHead,Cont),
	%arg(1,Ans,Resul),
	( 
	    Gconj == true ->
	    Clauses = [(NewHead :- '$meta_call'(Cont))]    %like bridge, but Ans is a meta_call
	; 
 	    Clauses = [(NewHead :- Gconj, '$meta_call'(Cont))]
	).

convert_bridged_body(Blist,NewHead,Delay,Gconj,Ans,HeadVars,Cid,Clauses0,ListVars,ListAux) :-
	get_transcall_bridge(Blist,HeadVars,Delay,Cid,TRANSCall,Cont,Delay1,ListVars,ListAux,NewHead),
	( Gconj == true ->
	  Clauses0 = [(NewHead :- TRANSCall)|Clauses1]
	; Clauses0 = [(NewHead :- Gconj,TRANSCall)|Clauses1]
	),
	Blist = [_|Rest],
	extract_guard(Rest,NGuard,[],NBlist,Clauses1,Clauses2),
	list_to_conj(NGuard,NGconj),
	convert_bridged_body(NBlist,Cont,Delay1,NGconj,Ans,HeadVars,Cid,Clauses2,ListAux,_ListAux2).

noList([]) :- !, fail.
noList([_|_]) :- !, fail.
noList(_).

get_transcall(Blist,HeadVars,_Delay,Cid,TRANSCall,Cont,_Delay1,ListVars,Diff,NewHead) :-
	Blist = [Lit|Rest],
	Lit = Call,
	functor(Call,P_BorT,A),
	(
	    'trans$bridge'(P_BorT/A) -> 
	    get_pred_bridge(Call,ContBridgeName),
	    functor(ContBridge,ContBridgeName,3),
	    arg(1,ContBridge,Call),
	    arg(2,ContBridge,Cid),
	    arg(3,ContBridge,Cont2),
	    get_cont_pred(NewHead,ContPred,_),
	    TRANSCall = (ContBridge)
	;
	    %añadir_un_argumento_con_las_continuaciones_bridge,
	    vars_in_term(Blist,_BodyVars),
	    get_cont_pred(NewHead,ContPred,ContPredMod),
	    get_pred_init(Call,PredInit),
	    (functor(NewHead,_,3), arg(3,NewHead,PrevCont), noList(PrevCont); true),
	    TRANSCall = ( 
			    tabled_call(Call, Sid, PredInit, PT, 1),
			    (
				'$meta_call'(PT) ->
				true
			    ;
				resume_ccalls(Sid,CCall,0,0,0),
				'$meta_call'(CCall)
			    ),
			    new_ccall(Cid,Sid,Diff,ContPredMod,PrevCont,F),
			    '$meta_call'(F)
			)
	),
	functor(Cont,ContPred,3),
	arg(1,Cont,Call),
        getArgs(Call,1,A,ListArgs),
	lists:reverse(HeadVars,HeadVars1),
	append(HeadVars1,ListArgs,NewList),
	repeat(NewList,NewList1),
	lists:reverse(NewList1,NewList2),
	vars_in_term(Call,CallVars),
	append(CallVars,ListVars,LVarsAux),
	repeat(LVarsAux,LVarsFinal),
	vars_in_term(Rest,RestVars),
	intersection(LVarsFinal,RestVars,MoreVars),
	append(MoreVars,NewList2,ListFinal),
	repeat(ListFinal,ListFinal1),
	lists:reverse(ListFinal1,Diff),
	arg(2,Cont,Cid),
	arg(3,Cont,Diff),
	insert_module(Cont,Cont2).

get_transcall_bridge(Blist,HeadVars,_Delay,Cid,TRANSCall,Cont,_Delay1,ListVars,Diff,Ans) :-
	Blist = [Lit|Rest],
	Lit = Call,
	functor(Call,P_BorT,A),
	arg(3,Ans,LCont),
	(
	    'trans$bridge'(P_BorT/A) -> 
	    get_pred_bridge(Call,ContBridgeName),
	    functor(ContBridge,ContBridgeName,3),
	    arg(1,ContBridge,Call),
	    arg(2,ContBridge,Cid),
	    arg(3,ContBridge,Cont2),
	    get_cont_pred(Ans,ContPred,_),
	    TRANSCall = (ContBridge)
	;
	    %añadir_un_argumento_con_las_continuaciones_bridge,
	    vars_in_term(Blist,_BodyVars),
	    get_cont_pred(Ans,ContPred,ContPredMod),
	    get_pred_init(Call,PredInit),
	    arg(3,Ans,PrevCont),
	    TRANSCall = ( 
			    tabled_call(Call, Sid, PredInit, PT, 1),
			    (
				'$meta_call'(PT) ->
				true
			    ;
				resume_ccalls(Sid,CCall,0,0,0),
				'$meta_call'(CCall)
			    ),
			    new_ccall(Cid,Sid,Diff,ContPredMod,PrevCont,F),
			    '$meta_call'(F)
			)
	),
	functor(Cont,ContPred,4),
	arg(1,Cont,Call),
        getArgs(Call,1,A,ListArgs),
	lists:reverse(HeadVars,HeadVars1),
	append(HeadVars1,ListArgs,NewList),
	repeat(NewList,NewList1),
	lists:reverse(NewList1,NewList2),
	vars_in_term(Call,CallVars),
	append(CallVars,ListVars,LVarsAux),
	repeat(LVarsAux,LVarsFinal),
	vars_in_term(Rest,RestVars),
	intersection(LVarsFinal,RestVars,MoreVars),
	append(MoreVars,NewList2,ListFinal),
	repeat(ListFinal,ListFinal1),
	lists:reverse(ListFinal1,Diff),
	arg(2,Cont,Cid),
	arg(3,Cont,Diff),
	arg(4,Cont,LCont),
	insert_module2(Cont,Cont2).

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
	module_name(Mod),
	append(Mod,":",MAux),
	append(FName,"_",TmpName),
	append(TmpName,NName,Name),
	append(MAux,Name,ModName),
	name(ContPred,Name),
	name(ContPredMod,ModName).

insert_module(Cont,Cont2) :-
	functor(Cont,F,_),
	name(F,FName),	
	module_name(Module),
	append(Module,":",MAux),
	append(MAux,FName,FullName),
	name(Name,FullName),
	functor(Cont2,Name,3),
	arg(1,Cont,A1),
	arg(1,Cont2,A1),
	arg(2,Cont,A2),
	arg(2,Cont2,A2),
	arg(3,Cont,A3),
	arg(3,Cont2,A3).

insert_module2(Cont,Cont2) :-
	functor(Cont,F,_),
	name(F,FName),	
	module_name(Module),
	append(Module,":",MAux),
	append(MAux,FName,FullName),
	name(Name,FullName),
	functor(Cont2,Name,4),
	arg(1,Cont,A1),
	arg(1,Cont2,A1),
	arg(2,Cont,A2),
	arg(2,Cont2,A2),
	arg(3,Cont,A3),
	arg(3,Cont2,A3),
	arg(4,Cont,A4),
	arg(4,Cont2,A4).

get_pred_init(Call,ContPred) :-
	functor(Call,F,_),
        N1 is 0,
	name(N1,NName),
	name(F,FName),	
	module_name(Module),
	append(Module,":",MAux),
	append(MAux,FName,FullName),
	append(FullName,NName,Name),
	name(ContPred,Name).

get_pred_bridge(Call,ContPred) :-
	functor(Call,F,_),
        N1 is 0,
	name(N1,NName),
	name(F,FName),	
	append(FName,NName,Name),
	name(ContPred,Name).


convert_tabled_body_6(_HeadVars,_Cid,Ans,CHead,Blist,Clauses0) :-
	display('EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE'), nl,
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
	  ; 'trans$bridge'(P/A) ->
	    G0 = G,
	    Rest = [Lit|List],
	    Cls0 = Cls
	  ; 'trans$prolog'(Pred) ->
	    G0 = [Lit|G1],
	    extract_guard(List,G1,G,Rest,Cls0,Cls)
	  ; 'trans$default'((prolog)) ->
	    G0 = [Lit|G1],
	    extract_guard(List,G1,G,Rest,Cls0,Cls)
	  ; 
	    display('EHHH DEFAULT 2!!!'), nl
	  )
        ).

list_to_conj([],true).
list_to_conj([Lit|List],G0) :-	
	( List == [] ->
	  G0 = Lit
        ; G0 = (Lit,G),
	  list_to_conj(List,G)
        ).

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


