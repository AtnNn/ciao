:- module(memo_tr,
        [
            do_term_expansion/3
        ]).

:- use_module(library(lists), 
	[
	    reverse/2,
	    append/3
	]).

:- dynamic 'memo$cont'/1.
:- dynamic 'trans$cont'/1.

get_memo_clauses(L,Clauses) :-
	retract('memo$cont'(Clause)), !,
	get_memo_clauses([Clause|L],Clauses).
get_memo_clauses(Clauses,Clauses).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_term_expansion(0,_,_) :-
	retractall('memo$cont'(_)),
        retractall('trans$cont'(_)), assert('trans$cont'(0)).

do_term_expansion(end_of_file,_,_).

do_term_expansion(':-'(Com),':-'(Com),_) :- !.

do_term_expansion(Clause,[Clause0|Clause1],_) :-
	( 
	    Clause = (Head :- Body) -> 
	    true
	; 
	    Head = Clause, 
	    Body = true 
	),
	convert_clause(Head,Body,Clause0),
	get_memo_clauses([],ClauseMemo),
 %% 	add_discontiguous(ClauseMemo,Head,Clause1).
 	add_discontiguous(ClauseMemo,Head,Clause1),
   	display([Clause0|Clause1]), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_discontiguous([],_,[]).

add_discontiguous(C,H,[':-'(discontiguous(P/A))|C]) :-
	functor(H,P,A).	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_clause(Head,Body,Clause) :-
	translate_memos(Body, Body0),
	Clause = (Head :- Body0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_memos(true,true) :- !.

translate_memos((T,Body),(T0,Body0)) :- !, 
	translate_memo(T,T0),
	translate_memos(Body,Body0).

translate_memos(T,T0) :-
	translate_memo(T,T0).

%%%%%%%%%%%%%%%%%%%%%%%

translate_memo(memo(T), T0) :- !, 
	new_trans_memo(T,T0),
	Clause = (T0 :- 
		     memo_call(T0, Sid, TR),
		     (
			 var(TR) ->
			 (T, new_answer(T0,Sid); set_complete(Sid))
		     ;
			 consume_answer(T0,Sid,0)
		     )
	         ),
	assert('memo$cont'(Clause)).

translate_memo(T, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_trans_memo(Head,NewHead) :-
	functor(Head,P,A),
	name(P,Pl),
	retract('trans$cont'(N)),
	N1 is N + 1,
	assert('trans$cont'(N1)),
	name(N,NName),
        append(Pl,NName,MName),
	name(Npred,MName),
	functor(NewHead,Npred,A),
	set_args(Head, NewHead,A).

set_args(_,_,0) :- !.
set_args(H,NewH,N) :- 
        arg(N,H,Arg),
        arg(N,NewH,Arg),
	N1 is N - 1,
	set_args(H,NewH,N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %% trans_built_in(memo_call(_,_)).
 %% trans_built_in(new_answer(_,_)).
 %% trans_built_in(consume_answer(_,_)).


