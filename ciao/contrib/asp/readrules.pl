:- module(readrules, [read_file/3]).

:- use_module(library(strings)).
:- use_module(library(atom2term)).
:- use_module(library(asp(misc))).
:- use_module(library(asp(error))).
:- include('define.pl').
%:- data last_char/1.

:- data prev/1, currLine/1.
:- data eor/1.
:- data lineNo/1.
:- data par/1, doubleq/1, singleq/1.
:- data prologComment/0.

prev([]).

read_file(Type,S, LineTerm) :-
	initReadASP,
	repeat,
	get_line(S, String),
%	(String=end_of_file -> display(String);write_string(String)), nl,
	splitLine(Type,String),
%	display('CurrLine: \n'),
%	prtCurrLine,
%	display('-----------------------------\n'),
	(String=end_of_file -> LineTerm=end_of_file;
	    makeTerm(Type,LineTerm)).

splitLine(_,end_of_file) :- !.
splitLine(_,[]) :- !.
splitLine(Type,Line) :-
	(prologComment -> endComment(Type,Line,String);String=Line),
%	display('Prev:'), nl,
%	prtPrev,
%	display('------------------\n'),
	prev(Str),
	save_prev([]),
	append(Str, String, String1),
	resetDoubleQ, resetPar, resetSingleQ,
	((singleq(0),doubleq(0)) -> rem_start_space(String1, String2);
	    true),
%	display('splitLine: String2='), write_string(String2),nl,
	check_end_term(Type,String2, String3, Rest),
%	display('splitLine: String3='), write_string(String3),nl,
%	display('splitLine: Rest='), write_string(Rest),nl,
%	(eor(1) -> display('eor(1)');display('eor(0)')), nl,
	(eor(1) -> (append("(",String3,String4),
	 assertz_fact(currLine(String4)),save_eor(0));
	    save_prev(String3)),
	splitLine(Type,Rest).

endComment(prolog,[],[]) :- !, fail.
endComment(prolog,[0'*,0'\\|T], T) :- !,
%	display('end of comment found\n'),
	retractall_fact(prologComment).
endComment(prolog,[_|T],R) :- endComment(prolog,T,R).
endComment(asp,S,S).

makeTerm(Type,LineTerm) :-
	currLine(Str),
	(Type=asp -> convert2prologTerm(Str,Str1);Str1=Str),
%	 write_string(Str1), nl,
	convert_term(Type,Str1,LineTerm).
makeTerm(_,_) :- retractall_fact(currLine(_)), !, fail.

% convert string into either atom or term.
convert_term(asp,String, Term) :-
%	display('1\n'),
	append("(#const ", _, String), !,
	name(Term, String).
convert_term(asp,String, Term) :-
%	display('1\n'),
	append("(#hide ", _, String), !,
	name(Term, String).
convert_term(asp,String, Term) :-
%	display('1\n'),
	append("(#weight ", _, String), !,
	name(Term, String).
convert_term(asp,String, Term) :-
%	display('1\n'),
	append("(#domain ", _, String), !,
	name(Term, String).
convert_term(asp,String, Term) :-
%	display('2\n'),
	append("(compute ", S1, String), !,
	append(N,S0,S1),
%	write_string(S0), nl,
%	write_string(N), nl,
	append("^",S2,S0),
%	write_string(S2), nl,
	append(S3,"#.",S2),
%	write_string(S3), nl,
	append(S3,".",S4),
	string_to_term(S4,T3),
	string2term(N,NT),
%	display('NT='),display(NT), nl,
	(N=[]-> Term=compute(#(^(T3)));Term=compute(#(^(NT,T3)))).
convert_term(asp,String,Term) :-
%	display('3\n'),
	append("(#import",Str1,String),
	append("('#import'",Str1,Str2),
	!, string_to_term(Str2, Term).
%	!,string_to_term(Str2, Term).
convert_term(asp,String,Term) :-
%	display('4\n'),
%	read(_),
%	display(String), nl,
%	write_string(String), nl, read(_),
	append("(#use_asp",Str1,String),
%	display('Str1='), write_string(Str1), nl, read(_),
	append("('#use_asp'",Str1,Str2),
%	write_string(Str2), nl, read(_),
%	display('calling string to term\n'),
	!,string_to_term(Str2, Term).
convert_term(_,String, Term) :-
%	display('5\n'),
	string_to_term(String, Term).
%   display('Term1='),
%   prt_term(Term), nl.

% prev data handler:
rem_prev :- retractall_fact(prev(_)), !.
rem_prev.

save_prev(String) :- rem_prev, assertz_fact(prev(String)), !.
% -----------------------------------------------
% eor handler:
rem_eor :- retractall_fact(eor(_)), !.
rem_eor.

save_eor(X) :- rem_eor, assertz_fact(eor(X)).
% ------------------------------------------------

% remove the begining space and stop at the first non-space char.
rem_start_space([], []) :- !.
rem_start_space([C1|T1], T2) :-
	(space(C1);tabb(C1)), !,
	 rem_start_space(T1, T2).
rem_start_space(T1, T1).
%---------------------------------------------

% check end of rule exists..........
check_end_term(_,[], [], []).
check_end_term(Type,[0'"|T1], [0'"|T2], T3) :- !,
%	display('1\n'),
	change_doubleq,
	check_end_term(Type,T1, T2, T3).
check_end_term(prolog,[H|T1], [H|T2], T3) :-
	single_quote(H), !,
%	display('2\n'),
	change_singleq,
	check_end_term(prolog,T1, T2, T3).
%check_end_term(asp,[H|T1], [H|T2], T3) :- 
%	single_quote(H), !,
%	display('3:'),
%	check_end_term(asp,T1,T2,T3).
check_end_term(Type,[0'(|T1], [0'(|T2], T3) :- !,
%	display('4\n'),
	inc_par,
	check_end_term(Type,T1, T2, T3).
check_end_term(Type,[0')|T1], [0')|T2], T3) :- !,
%	display('5\n'),
	dec_par,
	check_end_term(Type,T1, T2, T3).
check_end_term(_,[0'%|_],[],[]) :- 
	doubleq(0),!.
%	display('6\n'),
check_end_term(prolog,[0'\\,0'*|T],String, Rest) :- !,
%	display('7\n'),
	assertz_fact(prologComment),
	endComment(prolog,T,T1),
	check_end_term(prolog,T1,String,Rest).
check_end_term(_,[0'.], [0'),0'.], []) :-
%	display('8\n'),
%	(doubleq(0)-> display('doubleq(0)');display('doubleq(1)')), nl,
	 doubleq(0),
	 singleq(0),
%	 (par(0)-> display('par(0)');display('par(1)')), nl,
	 par(0), !, 
	 save_eor(1).
check_end_term(_,[0'., C2|T], [0'),0'.], T) :-
%	display('9\n'),
	(space(C2);tabb(C2)),
%       (doubleq(0)-> display('doubleq(0)');display('doubleq(1)')), nl,
	doubleq(0),
	singleq(0),
%	(par(0)-> display('par(0)');display('par(1)')), nl,
	par(0), !,
	save_eor(1).
check_end_term(_,[C1|T1], [C1|T2], T3) :-
%	display('10\n'),
	!, check_end_term(_,T1, T2, T3).
% -----------------------------------------------

convert2prologTerm(String,String1) :-
%	display('now conver2prologTerm: String='), display(String),nl,
      convert_special(String,String1).
%      display('convert_special='), write_string(String1), nl.

convert_special([], []).
convert_special([0'{|T], [0'^|T1]) :- !,
	convert_special(T, T1).
convert_special([0'}|T], [0'#|T1]) :- !,
	convert_special(T, T1).
convert_special([0'[|T], [0'&|T1]) :- !,
	convert_special(T, T1).
convert_special([0']|T], [0'@|T1]) :- !,
	convert_special(T, T1).
convert_special([0'"|T], [H|T1]) :- !,
	single_quote(H),
	convert_special(T,T1).
convert_special([H|T], [H|T1]) :-
	 convert_special(T, T1).
%-------------------------------------------------

resetDoubleQ :- remDoubleQ, assertz_fact(doubleq(0)).
change_doubleq :- doubleq(X), X1 is 1-X,
	remDoubleQ,
	assertz_fact(doubleq(X1)).
remDoubleQ :- retractall_fact(doubleq(_)), !.
remDoubleQ.

resetSingleQ :- remSingleQ, assertz_fact(singleq(0)).
change_singleq :- singleq(X), X1 is 1-X,
	remSingleQ,
	assertz_fact(singleq(X1)).
remSingleQ :- retractall_fact(singleq(_)), !.
remSingleQ.


resetPar :- remPar, assertz_fact(par(0)).
inc_par :- par(X), X1 is X+1,
	retract_fact(par(X)),
	assertz_fact(par(X1)).
dec_par :- par(X), X>0, !, X1 is X-1,
	remPar,
	assertz_fact(par(X1)).
dec_par :- this_module(M),
	err(_,M,'Error in reading file',0).
remPar :- retractall_fact(par(_)), !.
remPar.

%----------------------------------------------------------------------
% Predicates to handle the line number of ASP module.
setLineNo(N) :- retractall_fact(lineNo(_)),
	assertz_fact(lineNo(N)).

% incLineNo :- lineNo(N), N1 is N+1,
% 	setLineNo(N1).

initReadASP :-
	setLineNo(1),
	resetDoubleQ,
	resetSingleQ,
	resetPar,
	save_eor(0),
	save_prev([]).

%-------------------------------------------------
% ----------- end reading ---------------------------------------------------


% we do not need the code below any more.

% ------------- Start  of  reverse back to normal format. ------
% reverse format '{' --> '^', '}' --> '#', '[' --> '&', ']' --> '@'.
/*
reverse_format(Atm, String) :-
   atom(Atm), 
%   message(['Atom']), !,
   name(Atm, String1),
   rem_last_dot(String1, String2),
   reverse_convert(String2, String).
reverse_format(Term, String) :-
      term(Term), !,
      create_dict(Term, D),
      D=dic(L, B),
      rename_dict(B, B1, 65),
      D1=dic(L,B1),
      rename(Term, D1),
      term_to_string(Term, String1),
      reverse_convert(String1, String).


reverse_convert([], []).
reverse_convert([H1|T1], [H2|T2]) :-
   power(H1),
   left_brace(H2), !,
   reverse_convert(T1,T2).
reverse_convert([H1|T1], [H2|T2]) :-
   hash(H1),
   right_brace(H2), !,
   reverse_convert(T1,T2).
reverse_convert([H1|T1], [H2|T2]) :-
   andc(H1),
   left_bracket(H2), !,
   reverse_convert(T1,T2).
reverse_convert([H1|T1], [H2|T2]) :-
   ats(H1),
   right_bracket(H2), !,
   reverse_convert(T1,T2).
reverse_convert([H1|T1], [H1|T2]) :-
   !, reverse_convert(T1,T2).

%-----------------------------------------

% remove last dot from string
rem_last_dot([], []).
rem_last_dot([H], []) :-
   dot(H), !.
rem_last_dot([H, C|_], []) :-
   dot(H), (space(C);tabb(C)), !.
rem_last_dot([H|T], [H|T1]) :-
   rem_last_dot(T, T1).
% -------------------------------------

% use dictionary to rename variables: A, B, ..., Z.
rename_dict([], [], _).
rename_dict([_|T], [C|T1], X) :-
	X1 is X+1,
	char_code(C, X),
	rename_dict(T, T1, X1).
*/
% ---------------------------------------------------------------
% --------------- end reverse back to normal rules ---------------

