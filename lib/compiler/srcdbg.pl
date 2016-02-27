:- module(srcdbg, [srcdbg_expand/4], [assertions]).

:- use_module(library('compiler/c_itf'), [location/3]).
:- use_module(library(lists),[delete/3,length/2]).
:- use_module(library(sets),[insert/3]).
:- use_module(library(write),[printable_char/1]).

:- pred srcdbg_expand/4 # "This is the expansion needed to perform
   source-level debugging.".

srcdbg_expand(Old_H,Old_B,Old_H,New_B) :-
	location(Src,L0,L1),
	search_head(Old_H,Xs),
	srcdbg_expand_(Old_B,New_B,Src,L0,L1,Xs,_).

srcdbg_expand_(','(A,B),','(NewA,NewB),Src,L0,L1,Xs,Zs):- 
	!,
	srcdbg_expand_(A,NewA,Src,L0,L1,Xs,Ys),
	srcdbg_expand_(B,NewB,Src,L0,L1,Ys,Zs).
srcdbg_expand_('->'(A,B),'->'(NewA,NewB),Src,L0,L1,Xs,Zs):- 
	!,
	srcdbg_expand_(A,NewA,Src,L0,L1,Xs,Ys),
	srcdbg_expand_(B,NewB,Src,L0,L1,Ys,Zs).
srcdbg_expand_(';'(A,B),';'(NewA,NewB),Src,L0,L1,Xs,Zs):- 
	!,
	srcdbg_expand_(A,NewA,Src,L0,L1,Xs,Ys),
	srcdbg_expand_(B,NewB,Src,L0,L1,Ys,Zs).
srcdbg_expand_(if(A,B,C),if(NewA,NewB,NewC),Src,L0,L1,Xs,Ks):- 
	!,
	srcdbg_expand_(A,NewA,Src,L0,L1,Xs,Ys),
	srcdbg_expand_(B,NewB,Src,L0,L1,Ys,Zs),
	srcdbg_expand_(C,NewC,Src,L0,L1,Zs,Ks).
srcdbg_expand_(\+(A),\+(NewA),Src,L0,L1,Xs,Ys):- 
	!,
	srcdbg_expand_(A,NewA,Src,L0,L1,Xs,Ys).
srcdbg_expand_(!,!,_,_,_,Xs,Xs):- !. 
srcdbg_expand_(true,true,_,_,_,Xs,Xs):- !. 
srcdbg_expand_(Goal,srcdbg_spy(Goal,Pred,Src,L0,L1,Number),Src,L0,L1,Xs,Zs):- 
	!,
	functor(Goal,Pred,Arity),
	add_pred_to_list(Pred,Xs,Ks),
	get_pred_number(Pred,Ks,Number),
	search_args(1,Ks,Goal,Arity,Zs).

%% search_head: Add pred to the list and all arguments
search_head(Goal,Xs):-
	functor(Goal,Pred,Arity),
	search_args(1,[pred(Pred,1)],Goal,Arity,Xs).

%% search_args: Search the whole list of arguments. It calls 
%% search_arg which add individual arguments to the list of strings
search_args(_,Xs,_,0,Xs).
search_args(Number,Xs,Goal,Number,Ys):-
	!,
	arg(Number,Goal,Arg),
	search_arg(Arg,Xs,Ys).
search_args(Number,Xs,Goal,Arity,Ys):-
	arg(Number,Goal,Arg),
	search_arg(Arg,Xs,Zs),
	Next_arg is Number+1,
	search_args(Next_arg,Zs,Goal,Arity,Ys).

% % Caso base: Aridad 0
% search_preds(_,Xs,_,0,Xs):- !.

% % Last arguments of the Goal
% search_preds(Max,Xs,Goal,Max,Ys):-
% 	!,arg(Max,Goal,Arg),
% 	search_arg(Arg,Xs,Ys).

% % Arguments from 1 to MaxNumber-1 of the Goal
% search_preds(Number,Xs,Goal,MaxNumber,Zs):-
% 	Number < MaxNumber,
% 	!,arg(Number,Goal,Arg),
% 	search_arg(Arg,Xs,Ys),
% 	NewNumber is Number+1,
% 	search_preds(NewNumber,Ys,Goal,MaxNumber,Zs).

%% search_arg: Add argument to the list. Search_arg has posibility of 
%% arguments

% Variables
search_arg(Arg,Xs,Xs):- var(Arg),!.
% Strings
search_arg([X|Xs],Ys,Zs):- 
	is_string([X|Xs]), !,
	atom_codes(Word,[X|Xs]),
	add_pred_to_list(Word,Ys,Zs).
% Empty list
search_arg([],Xs,Xs):- !.
% Numbers
search_arg(Arg,Xs,Xs):- number(Arg).
% Atoms
search_arg(Arg,Xs,Ys):-
	nonvar(Arg),
	atom(Arg),
	add_pred_to_list(Arg,Xs,Ys).
% Lists
search_arg(.(X,Y),Ys,Zs):-
	!, 
	search_arg(X,Ys,Ks),
	search_arg(Y,Ks,Zs).
% Preds
search_arg(Arg,Xs,Ys):-
	functor(Arg,Pred,Arity),
	Arity > 0,
	add_pred_to_list(Pred,Xs,Zs),
	search_args(1,Zs,Arg,Arity,Ys).

% %% SEARCH_ARG
% % Variables
% search_arg(Arg,Xs,Xs):-
% 	var(Arg),!.

% % Strings
% search_arg([X|Xs],Ys,Zs):-
% 	is_string([X|Xs]),!,
% 	atom_codes(Word,[X|Xs]),
% 	add_pred_to_body(Word,Ys,Zs,_).
	
% % Empty list
% search_arg([],Xs,Xs).

% % Atoms
% search_arg(Arg,Xs,Ys):-
% 	nonvar(Arg),
% 	atm(Arg),!,
% 	add_pred_to_body(Arg,Xs,Ys,_).

% % Numbers
% search_arg(Arg,Xs,Xs):-
% 	nonvar(Arg),
% 	number(Arg),!.

% % Preds
% search_arg(Arg,Xs,Zs):-
% 	nonvar(Arg),
% 	functor(Arg,Pred,Arity),
% 	Arity > 0,!,
% 	search_arg(Pred,Xs,Ys),
% 	search_preds(1,Ys,Arg,Arity,Zs).



% add_pred_to_body  
% add_pred_to_body(Pred,Xs,Zs,R):-
% 	find_pred_suffix(Pred,Xs,Number),
% 	R is Number + 1,
% 	(member(pred(Pred,OldNumber),Xs) ->
% 	    delete(Xs,pred(Pred,OldNumber),Ys),
% 	    NewNumber is OldNumber + 1,
% 	    insert(Ys,pred(Pred,NewNumber),Zs)
% 	; NewNumber is Number+1,
% 	  insert(Xs,pred(Pred,NewNumber),Zs)).

add_pred_to_list(Pred,Xs,Ys):-
	(member(pred(Pred,OldNumber),Xs) ->
	    delete(Xs,pred(Pred,OldNumber),Zs),
	    NewNumber is OldNumber+1,
	    insert(Zs,pred(Pred,NewNumber),Ys)
	;
	    insert(Xs,pred(Pred,1),Ys)
	).

get_pred_number(_,[],0).
get_pred_number(Word,[pred(Pred,Number)|Xs],Occur_Number):-
	count_suffix(Word,Pred,Suffix_Number),
	Count_Number is Number * Suffix_Number,
	get_pred_number(Word,Xs,Rest_Number),
	Occur_Number is Count_Number + Rest_Number.
	
% %find_pred_suffix
% find_pred_suffix(_,[],0).

% find_pred_suffix(Word,[pred(Pred,OldNumber)|Xs],Number):-
%    	count_suffix(Word,Pred,NumberPred),
% 	NewNumber is NumberPred * OldNumber,
% 	find_pred_suffix(Word,Xs,NumberFind),
% 	Number is NewNumber + NumberFind.  


%count_suffix
count_suffix(Word,Pred,Number):-
	atom_codes(Word,WordList),
	atom_codes(Pred,PredList),
	find_sublist(WordList,PredList,Number,WordList).

find_sublist(_,[],0,_):-!.
find_sublist(WordList,WordList,1,_):-!.
find_sublist(WordList,PredList,0,_):-
	length(WordList,WordNumber),
	length(PredList,PredNumber),
	WordNumber > PredNumber,!.
find_sublist([],Ys,Number,Word):-
	find_sublist(Word,Ys,NewNumber,Word),!,
	Number is NewNumber + 1.
find_sublist([X|Xs],[X|Ys],Number,Word):-
	find_sublist(Xs,Ys,Number,Word),!.
find_sublist([X|Xs],[_|Ys],Number,Word):-
	find_sublist([X|Xs],Ys,Number,Word),!.

%is string
is_string([X]):-printable_char(X).
is_string([X|Xs]):-
	printable_char(X),
	is_string(Xs).

:- comment(version_maintenance,dir('../../version/')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*5+24,1999/12/28,13:18*53+'CET'), "Fixed bug in number
   of pred to search in emacs (Manuel Carlos Rodriguez)").












