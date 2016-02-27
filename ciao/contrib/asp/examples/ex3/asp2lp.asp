:- module('asp2lp.asp', [compute/2]).

:- export(assert/1).
:- export(retract/1).
:- export(model/1).
:- export(change_parm/1).
:- export(assert_nb/1).
:- export(retract_nb/1).
:- export(reset_asp/0).
:- export(release/1).
:- use_package([objects]).
:- use_module(library(lists)).
:- use_module(library(strings)).
:- use_module(library(system)).
:- use_module(library(atom2term)).
:- use_module(library(operators)).
:- use_module(library(streams)).
:- use_module(engine(internals)).
:- use_module(library(write)).
:- use_module(library(davinci)).
:- use_module(library(terms_check)).
:- use_module(library(vndict)).
:- use_module(library(iso_byte_char)).
:- use_module(library(terms)).
:- use_module(library(filenames)).

:- use_class(justclass).
:- use_class(wellfound).
:- use_class(atomclass).
:- use_class(skepclass).
self('asp2lp.asp').

aspFileName('/home/pippo2/NEWCIAO/CiaoDE/ciao/contrib/asp/examples/ex3/asp2.lp').

 :- export(getAspName/1).
getAspName(F2) :- aspFileName(F),
	absolute_file_name(F,F1),
	no_path_file_name(F1,F2).

children([prolog('p3','p3')]).
:- use_module('/home/pippo2/NEWCIAO/CiaoDE/ciao/contrib/asp/examples/ex3/p3.pl').
parent(['asp1lp.asp']).
comma(44).
period(46).

lpar(40).
rpar(41).
under_score(95).


% atom: store atom index, atom(index,name)
% rule : store rule index, rule(Index,head,body)
% model: store models, model(no,atom_name, value, just
% skepAtoms/2: store skeptical atoms
% ans/2:stores answer models list for skeptical coomputation
:- dynamic atom/2, rule/3, model/4.
:- dynamic asp_model/3, uf/2, rr/1, comp/2.
:- dynamic lparse_set/2.
:- dynamic computed/1, rule_computed/1.
:- dynamic graph_list/1.
% smodels_term_status: flag of end_of_state.
:- dynamic smodels_term_status/1.
% mftime: used to store time of last change for ASP program
% state: record state history
:- data mftime/1.
% ans: used in skeptical computation
:- data ans/2.
% end_model: store if all models is stored in classes.
:- data end_models/1.
:- op(550, xfy, '..').
:- op(550, xfy, :).
:- op(700, xf, !).
:- op(990, fx, not).
:- op(1100, xfy, ^).
:- op(1100, fy, ^).
:- op(1102, xfy, #).
:- op(1102, yf, #).
:- op(1100, xfy, &).
:- op(1100, fy, &).
:- op(1102, xfy, @).
:- op(1102, xf, @).
%:- op(99, fy, '!').    % this one might not be needed.
% ----------------------------------------------------------------

% defining some char ascii codes: -------------------------------
percent(37).
space(32).
tabb(9).
double_quote(34).
single_quote(39).
left_par(40).
right_par(41).
dot(46).
left_bracket(91).
right_bracket(93).
left_brace(123).
right_brace(125).
power(94).
hash(35).
andc(38).
ats(64).
% ---------------------------------------------

% fn: stores the filename and other stuff.
% compute_number: a field to store the number should be in the compute
%                 statment.
% old_compute: save old compute values Number and string.
% retracted_list: a list of rules that is retracted from the asp file.
:- data compute_number/2.
:- data old_compute/2.


% -------------------------------------------

smodel_parameter(nolookahead).
smodel_parameter(backjump).
% -----------------------------------------------------


lparse_parm(['-d ',all,' -W ',none, ' ']).
skep_parm(['0','skeptical','-asp_prolog']).
asp_prolog_parm(['-asp_prolog']).
trace_parm(['0','-trace','-asp_prolog']).
lparse_exe('/home/grad3/okhatib/lparse-1.0.17/src/lparse ').
smodel_exe('/home/grad3/okhatib/smodels-2.28/smodels ').
%------------------------------------------------
%running Smodels System:
%------------------------
runSolver(No,OSM,Type) :- 
   aspFileName(AspF),
	children(L), (L=[]->F=AspF;atom_concat(AspF,'1',F)),
   lparse_set(LPARM2,_),
   lparse_parm(LPARM1),
   append(LPARM1,LPARM2, LPARM3),
   append(LPARM3,[F], LPARM),
   atoms_concat(LPARM, LP),
	this_module(M),
	ERR='running lparse: lparse does not exist',
   number_atom(No,Num),
   lparse_set(_,SPARM2),
	atoms_concat(SPARM2,SP),
	smodel_exe(SMEXE),
	(Type=4->atoms_concat([SMEXE,' -w ', SP],X1);
	atoms_concat([SMEXE,' ',Num,' ',SP],X1)),
	lparse_exe(LPEXE),
	atoms_concat([LPEXE,LP,' | ',X1],RUNSMODELS),
   catch(popen(RUNSMODELS,read,OSM),_,err(0,M,ERR,0)).
stop_solver :-
	this_module(M), 
	aspFileName(AspF),
	atom_concat('Smodels when reading asp file: ',AspF, ERR),
	err(0,M,ERR,0).
get_parm(asp_prolog,P) :- asp_prolog_parm(P).
get_parm(skep,P) :- skep_parm(P).
get_parm(trace,P) :- trace_parm(P).

get_one_line(O,String) :-
	this_module(M),
   catch(get_line(O,String), E, err(E,M,' in read_lparse_send_smodels',0)).

:- data prev/1, currLine/1, ruleString/1.
:- data eor/1.
:- data lineNo/1.
:- data last_char/1.
:- data par/1, doubleq/1.
prev([]).

read_file(S, LineTerm, V) :-
	initReadASP,
	repeat,
	get_line(S, String),
	(String=[]->assertz_fact(ruleString([]));true),
	splitLine(String),
	(String=end_of_file -> (LineTerm=end_of_file,V=[]);
	    makeTerm(LineTerm,V)).

splitLine(end_of_file) :- !.
splitLine([]) :- !.
splitLine(String) :-
	prev(Str),
	save_prev([]),
	append(Str, String, String1),
	resetDoubleQ, resetPar, 
	(doubleq(0)-> rem_start_space(String1,String2);true),
	check_end_term(String2, String3, Rest),
	(eor(1) -> (append("(",String3,String4),
	assertz_fact(currLine(String4)),
	save_eor(0));
	    save_prev(String3)),
	splitLine(Rest).

makeTerm(LineTerm, V) :-
	currLine(Str),
	makePrologTerm(Str, LineTerm,V).

makeTerm(_,_) :- retractall_fact(currLine(_)), !, fail.

makePrologTerm(Str, LineTerm, V) :- 
	convert2prologTerm(Str,Str1),
	convert_term(Str1,LineTerm,V),!.
convert_term(String, Term,_) :-
	append("(#const ", Str1, String), !,
	append("'#const'(", Str1, Str2),
	!, string_to_term(Str2,Term).
convert_term(String, Term,_) :-
	append("(#hide ", Str1, String), !,
	append("'#hide'(", Str1, Str2),
	!, string_to_term(Str2,Term).
convert_term(String, Term,_) :-
	append("(#show ", Str1, String), !,
	append("'#show'(", Str1, Str2),
	!, string_to_term(Str2,Term).
convert_term(String, Term,_) :-
	append("(#show ", Str1, String), !,
	append("'#show'(", Str1, Str2),
	!, string_to_term(Str2,Term).
convert_term(String, Term,_) :-
	append("(#weight ", Str1, String), !,
	append("'#weight'(", Str1, Str2),
	!, string_to_term(Str2,Term).
convert_term(String, Term,_) :-
	append("(#weight ", Str1, String), !,
	append("'#weight'(", Str1, Str2),
	!, string_to_term(Str2,Term).
convert_term(String, Term,_) :-
	append("(compute ", S1, String), !,
	append(N,S0,S1),
	append("^",S2,S0),
	append(S3,"#).",S2),
	append(S3,".",S4),
	string_to_term(S4,T3),
	string2term(N,NT),
	(N=[]-> Term=compute(#(^(T3)));Term=compute(#(^(NT,T3)))).
convert_term(String,Term,_) :-
	append("(#import",Str1,String),
	append("('#import'",Str1,Str2),
	!,string_to_term(Str2, Term).
convert_term(String,Term,_V) :-
	append("(#use_asp",Str1,String),
	append("('#use_asp'",Str1,Str2),
	!,string_to_term(Str2, Term).
convert_term(String, Term,V) :-
	strToTerm(String, Term,V).
rem_prev :- retractall_fact(prev(_)), !.
rem_prev.

save_prev(String) :- rem_prev, assertz_fact(prev(String)), !.

rem_eor :- retractall_fact(eor(_)), !.
rem_eor.

save_eor(X) :- rem_eor, assertz_fact(eor(X)).

rem_start_space([], []) :- !.
rem_start_space([C1|T1], T2) :-
	(space(C1);tabb(C1)), !,
	 rem_start_space(T1, T2).
rem_start_space(T1, T1).

check_end_term([], [], []).
check_end_term([0'"|T1], [0'"|T2], T3) :- !,
      change_doubleq,
      check_end_term(T1, T2, T3).
check_end_term([0'(|T1], [0'(|T2], T3) :- !,
      inc_par,
      check_end_term(T1, T2, T3).
check_end_term([0')|T1], [0')|T2], T3) :- !,
      dec_par,
      check_end_term(T1, T2, T3).
check_end_term([0'%|T],[],[]) :- 
	doubleq(0),!,
	assertz_fact(ruleString([0'%|T])).
check_end_term([0'.], [0'),0'.], []) :-
	 doubleq(0), 
	 par(0), !, 
	 save_eor(1).
check_end_term([0'., C2|T], [0'),0'.], T) :-
      (space(C2);tabb(C2)),
	doubleq(0),
	par(0), !,
	save_eor(1).
check_end_term([C1|T1], [C1|T2], T3) :-
	!, check_end_term(T1, T2, T3).

convert2prologTerm(String,String1) :-
     convert_special(String,String1).

convert_special([], []).
convert_special([0'{|T], [0'^|T1]) :- !,
	convert_special(T, T1).
convert_special([0'}|T], [0'#|T1]) :- !,
	convert_special(T, T1).
convert_special([0'[|T], [0'&|T1]) :- !,
	convert_special(T, T1).
convert_special([0']|T], [0'@|T1]) :- !,
	convert_special(T, T1).
convert_special([0'"|T], [H,0'~|T1]) :- 
	doubleq(0), !,
	single_quote(H),
	change_doubleq,
	convert_special(T,T1).
convert_special([0'"|T], [0'~,H|T1]) :- 
	doubleq(1), !,
	single_quote(H),
	change_doubleq,
	convert_special(T,T1).
convert_special([H|T], [H|T1]) :-
	 convert_special(T, T1).

resetDoubleQ :- remDoubleQ, assertz_fact(doubleq(0)).
change_doubleq :- doubleq(X), X1 is 1-X,
	remDoubleQ,
	assertz_fact(doubleq(X1)).
remDoubleQ :- retractall_fact(doubleq(_)), !.
remDoubleQ.

resetPar :- remPar, assertz_fact(par(0)).
inc_par :- par(X), X1 is X+1,
	retract_fact(par(X)),
	assertz_fact(par(X1)).
dec_par :- par(X), X>0, !, X1 is X-1,
	remPar,
	assertz_fact(par(X1)).
dec_par :- message(['Error in reading file']), fail.
remPar :- retractall_fact(par(_)), !.
remPar.

setLineNo(N) :- retractall_fact(lineNo(_)),
	assertz_fact(lineNo(N)).
incLineNo :- lineNo(N), N1 is N+1,
	setLineNo(N1).

initReadASP :-
	setLineNo(1),
	resetDoubleQ,
	resetPar,
	save_eor(0),
	save_prev([]).

prtRuleString :-
	ruleString(Str),
	write_string(Str), nl,
	fail.
prtRuleString.

prtPrev :-
	prev(Str),
	write_string(Str), nl,
	fail.
prtPrev.

prtCurrLine :-
	currLine(Str),
	write_string(Str), nl,
	fail.
prtCurrLine.

% Assert a list of rules into ASP.
assert([]) :- !.
assert(L) :- %  spy(assert2), notrace,
	list(L), 
	assert_list(L,R),
       this_module(TT),
	module_concat(TT, assert2(R), M), 
        und(M).

assert_list(L,R) :- 
      aspFileName(X),
      open(X, append, S),
	assertList(S,L,R),
	close(S),
	this_module(TM),
	module_concat(TM,setStateChanged,TMCHANGED),
	'$meta_call'(TMCHANGED),
	incState.

makeV(25,['Z']).
makeV(N,[B|T]) :- 
	X is N+64,
	char_code(B,X),
	N1 is N+1,
	makeV(N1,T).

assignVar([],V,V).
assignVar([VHead|T],[VHead|VTail],V1) :-
	!,assignVar(T,VTail,V1).
assignVar([B|T],V,V1) :-
	atomic(B), !,
	assignVar(T,V,V1).
assignVar([B|T],V,V1) :-
	term(B),!,
	B=.. [_|Arg],
	assignVar(Arg,V,V2),
	assignVar(T,V2,V1).

assignV((H:-B),V) :-
	H =.. [_|T],
	assignVar(T,V,V1),
	assignV(B,V1).
assignV((B1,B2),V) :- 
	B1 =.. [_|T],
	assignVar(T,V,V1),
	assignV(B2,V1).
assignV(B,V) :- 
	B =.. [_|T],
	assignVar(T,V,_).

asp2prolog(H, H1, VV) :-
	(atom(H) -> name(H,Str);
	term_to_string(H,Str)), 
	(append(_,".",Str)-> Str2=Str;append(Str,".",Str2)),
	 makePrologTerm(Str2, H1, VV).

assertList(_,[],[]).
assertList(S,[H|T],[H1|T1]) :- 
	asp2prolog(H,H1,_VV),
	copy_term(H1,H2),
	makeV(1,V),
       assignV(H2,V),
	reverseFormat(H2,String3),
	write_string(S,String3), write(S,'.'), nl(S),!,

	assertList(S,T,T1).

append2ASPFile(R) :-
      aspFileName(X),
      open(X, append, S),
      append_to_file(S, R),
	close(S).

% assert2 is used on backtrack to restore the rules added to asp file.
assert2(L) :- 
	retract_list(L),
	decState.

%       trace,
% --------------------------------------------------------

% assert non backtrack... -------------------------
assert_nb([]) :- !.
assert_nb(L) :- list(L), 
      assert_list(L,_),
	self(ModuleName),
	atoms_concat([ModuleName,':',reset_asp], G),
	'$meta_call'(G).
% -------------------------------------------------

retract_list(L) :-
	aspFileName(ASPFile),
	atom_concat(ASPFile,'1',ASPTemp),
	open(ASPFile, read, ASPRead),
	open(ASPTemp, write, Tmp),
	read_file(ASPRead, LineTerm, V),
	termNotMember(LineTerm, L),
	write2ASPFile(Tmp,LineTerm, V),
	close(Tmp),
	close(ASPRead),
	copy_file(ASPTemp,ASPFile),
	!.

termNotMember(end_of_file, _) :- !.
termNotMember(_,[]) :- !.
termNotMember(LineTerm, [H|_]) :-
	ask(LineTerm, H), !,
	this_module(TM),
	module_concat(TM,setStateChanged,TMCHANGED),
	'$meta_call'(TMCHANGED),
	!,fail.

termNotMember(LineTerm, [_|T]) :-
	termNotMember(LineTerm, T),
	!.
:- use_module(library('make/system_extra')).

:- data retracted_list/1.

% retract a list of rules from asp file: ------------
retract([]) :- !.
retract(L) :- 
	list(L),
	asp2prologList(L,R),
	retractAsp(R),!,
	findall(L1, retracted_list(L1), R1),
       this_module(TT),
	module_concat(TT, retract2(R1), M), 
	und(M), !.

% retrace list of rules from ASP files.
retractAsp(R) :-
	aspFileName(FullName),
	atom_concat(FullName, '1', TmpAsp),
	open(FullName, read, FS),
	open(TmpAsp, write, TS),
	read_file(FS, LineTerm,V),
	matchTermList(LineTerm, R, TS,V), !,
	close(FS),
	close(TS),
	copy_file(TmpAsp, FullName),
	incState.
matchTermList(end_of_file, _, _,_V) :- !.
matchTermList(end, _, _,_V) :- !.
matchTermList(LineTerm,[],TS,V) :- !,
	write2ASPFile(TS, LineTerm,V),
	!,fail.
matchTermList(LineTerm, [H|_], _,_V) :-
	ask(LineTerm, H), !,
	assertz_fact(retracted_list(H)),
	this_module(TM),
	module_concat(TM,setStateChanged,TMCHANGED),
	'$meta_call'(TMCHANGED),
	!, fail.

matchTermList(LineTerm, [_|T], TS,V) :-
	matchTermList(LineTerm, T, TS,V),
	!, fail.
asp2prologList([], []).
asp2prologList([H|T], [H1|T1]) :-
	asp2prolog(H,H1,_VV),
	asp2prologList(T,T1).

% used on backtrack
retract2([]) :- !.
retract2(L) :-
	this_module(TM),
	module_concat(TM,setStateChanged,TMCHANGED),
	'$meta_call'(TMCHANGED),
       decState,
	append2ASPFile(L),!.

% retract non backtrack... -----------------------
retract_nb([]) :- !.
retract_nb(L) :- 
       list(L),
	asp2prologList(L,R),
	retractAsp(R),
	self(ModuleName),
	atoms_concat([ModuleName,':',reset_asp], G),
	'$meta_call'(G).
% -------------------------------------------------------
:- data aspCompute/1.

compute(No, L) :- var(L),var(No), !,
	retractall_fact(aspCompute(_)),
	aspFileName(ASPFile),
	open(ASPFile, read, ASPRead),
	read_file(ASPRead, LineTerm,_V),
	(LineTerm=compute(#(^(L)))->No=0;
	    (LineTerm=compute(#(^(No,L)))->true;fail)),
	     close(ASPRead),!.
compute(No, L) :- 
	retractall_fact(aspCompute(_)),
	aspFileName(ASPFile),
	open(ASPFile, read, ASPRead),
	atom_concat(ASPFile, '1', ASPTmp),
	open(ASPTmp, write, Tmp),
	read_file(ASPRead, LineTerm,V),
	A=compute(_),
	matchCompute(LineTerm, A, Tmp,V),
	getAspCompute(C),
       getCompute(C,No,L),
	\+write2ASPFile(Tmp,compute(No,L),[]),
	close(Tmp),
	close(ASPRead),
	copy_file(ASPTmp, ASPFile),

       this_module(TT),
	module_concat(TT, bkcomp(C), M), 
        und(M),
	self(ModuleName),
	atoms_concat([ModuleName,':',setStateChanged], G),
	'$meta_call'(G),
	incState.
getCompute(_,No,L) :-
	ground(No),ground(L),!.
getCompute(CParm,No,L) :-
	var(CParm),
	(var(No) -> No=0;true),
	 (var(L) -> L=[];true),!.
getCompute(compute(#(@(No1, L1))),No,L) :-
	(var(No) -> No=No1;true),
	 (var(L) -> L=L1;true),!.

matchCompute(end_of_file, _, _, _V) :- !.
matchCompute(end,_,_, _V) :- !.
matchCompute(LineTerm, H, _, _V) :-
	LineTerm=H, assertz_fact(aspCompute(LineTerm)), !, fail.

matchCompute(LineTerm,_,TS,V) :- !,
	write2ASPFile(TS, LineTerm,V),
	!,fail.

getAspCompute(C) :- aspCompute(C).
getAspCompute(_).

bkcomp(L) :- var(L),!.
bkcomp(L) :- 
	retractall_fact(aspCompute(_)),
	aspFileName(ASPFile),
	open(ASPFile, read, ASPRead),
	atom_concat(ASPFile, '1', ASPTmp),
	open(ASPTmp, write, Tmp),
	read_file(ASPRead, LineTerm,V),
	A=compute(_),
	matchCompute(LineTerm, A, Tmp,V),
	getAspCompute(C),
       getCompute(C,No,L),
	\+write2ASPFile(Tmp,compute(No,L),[]),
	close(Tmp),
	close(ASPRead),
	copy_file(ASPTmp, ASPFile),

	self(ModuleName),
	atoms_concat([ModuleName,':',setStateChanged], G),
	'$meta_call'(G),
	decState.
:- data parm/1.
parm([]).

lparse_set([],[]).
% change_param handler:
change_parm(L) :- 
       var(L), !, parm(L).
change_parm(L) :-!,
      retractall(lparse_set(_,_)),
      parse_arg(L, C1,C2),
      assertz(lparse_set(C1,C2)),
	(parm(OldParm)->true;OldParm=[]),
       this_module(TT),
	module_concat(TT, bkparm(OldParm), M), 
        und(M),
      retractall(parm(_)),
      assertz(parm(L)),
	self(ModuleName),
	atoms_concat([ModuleName,':',setStateChanged], G),
	'$meta_call'(G),
      incState.

parse_arg((lparse(L),smodels(S)),C1,C2) :-
	parse_lparse_arg(L,C1),
	parse_smodels_arg(S,C2).

parse_lparse_arg([], []).
parse_lparse_arg([H|T],C) :-
	atom(H), !,
	name(H,String),
	cnv2atom_list(String,Atom),
	parse_lparse_arg(T,C1),
	append(Atom,C1,C).
parse_lparse_arg([(H,V)|T],C) :-
    !, cnv_atom(V,A),
    atoms_concat([H,'=', A], Atom),
    parse_lparse_arg(T,C1),
    append(['-c', Atom], C1,C).

parse_smodels_arg([], []).
parse_smodels_arg([H|T],C) :-
	atom(H), !,
	name(H,String),
	cnv2atom_list(String,Atom),
	parse_smodels_arg(T,C1),
	append(Atom,C1,C).

bkparm([]) :- !.
bkparm(L) :- 
      retractall(lparse_set(_,_)),
      parse_arg(L, C1,C2),
      assertz(lparse_set(C1,C2)),
      retractall(parm(_)),
      assertz(parm(L)),
	self(ModuleName),
	atoms_concat([ModuleName,':',setStateChanged], G),
	'$meta_call'(G),
      decState.

'$meta_call'(X) :- call(X).
:- data aspModel/3, endModels/1.

:- export(aspModel/3).

model(Q) :- 
	retractall_fact(modelFound),
	getCState(State),
	!,model3(State,1,Q).
model(_).

model3(State,N,Q) :- 
	aspModel(State,N,Q).
model3(State,1,Q) :- 
	\+aspModel(State,1,Q),
	\+endModels(State),
	(existgroundASP -> true;groundAsp),
	getModel(State,1,Q), modelFound.
model3(State,No,Q) :- 
	No>1, \+aspModel(State,No,Q),
	\+endModels(State),
	existgroundASP,
	getModel(State,No,Q), modelFound.
model3(State,No,Q) :- 
	No>1, \+aspModel(State,No,Q),
	\+endModels(State),
	getCState(State1),
	State1=State,
	\+existgroundASP,
	groundAsp,
	getModel(State,No,Q), modelFound.
model3(State,No,Q) :-
	aspModel(State,No,_QQ),!,
	No1 is No+1,
	retractall_fact(modelFound),
	model3(State,No1,Q).
getModel(State,No,Q) :-
     currentAtom(State,_),
	runSolver(No,OSM,1),
	repeat,
	getSolverData(OSM,State,No,Q),!, close(OSM),
	(modelFound-> true;asserta_fact(endModels(State))),
	self(ModuleName),
	atoms_concat([ModuleName,':',resetStateChanged], G),
	'$meta_call'(G).
getSolverData(OSM,State,No,Q) :-
	retractall_fact(modelFound),
	repeat,
       get_line(OSM,L1),
	store_data(L1,State,No,Q), !.
:- data modelFound/0.

store_data("end.",_,_,_) :- !.
store_data(end_of_file,_,_,_) :- !.
store_data(L1,_,No,_) :- 
	append("Answer: ", L2, L1),
	string2term(L2,No),
	assertz_fact(modelFound),!,fail.
store_data(L1,State,No,Q) :- 
	modelFound, 
	append("Stable Model: ", L2, L1), 
	self(AspInterface),
	Q new justclass(AspInterface,No),
	stateAtom(State,A),
       parse1(L2,Q,A),
	assertz_fact(aspModel(State,No,Q)),
	Q:data_add(atomClass(A)),
	A:incRef,
	Q:getTrueAtoms(TAtoms),
	getFalseAtoms(Q,A,TAtoms),
	(tmpST(ST)->(Q:data_add(symboltableClass(ST)),ST:incRef);
	 true),!.
store_date(_,_,_,_):-!,fail.

parse1([],_,_):-!.
parse1(L,Q,A) :- 
	pos(" ",L,L3,L4),
	string2term(L3,Atom),
	A:atom(ANo,Atom),
	Q:data_add(just(ANo,1)),
	parse1(L4,Q,A),!.
getFalseAtoms(Q,A,TA) :-
	A:atom(N,Atom),
	\+member(Atom,TA),
	Q:data_add(just(N,0)), fail.
getFalseAtoms(_,_,_).

emptyJustClass(State,_,Q) :-
	pushEndModels(State),
	destroy(Q),!,fail.

aspModelExist(State) :- aspModel(State,_,_),!.

setEndModels(State) :- retractall_fact(endModels(State)),!,
	pushEndModels(State).
setEndModels(State) :- pushEndModels(State).

pushEndModels(State) :- asserta_fact(endModels(State)).
popEndModels(State) :- State>0,
	retractall_fact(endModels(State)),!.
popEndModels(_).

prtEndModels(State) :- endModels(State),!,
	display('endModels state='),display(State),nl.
prtEndModels(_) :- display('no endModels exists'),nl.

:- export(wfm/1).
:- data wfmState/2.

wfm(Q) :- 
	getCState(State),
	(wfmState(State,Q) -> true; wfm2(Q,State)).

wfm2(Q,St) :- 
	(existgroundASP->wfm3(Q,St);(groundAsp,wfm3(Q,St))).
wfm3(Q,State) :- 
	runSolver(0,OSM,4),
	repeat,
	getWellFound(OSM,State,Q),!, close(OSM),
	self(ModuleName),
	atoms_concat([ModuleName,':',resetStateChanged], G),
	'$meta_call'(G),!.

getWellFound(OSM,State,Q) :-
	currentAtom(State,A),
	repeat,
	get_line(OSM,String),
	positive_part(OSM,String,A,Q),!,
	Q:data_add(atomClass(A)),
	assertz_fact(wfmState(State,Q)),!.

positive_part(OSM,String,A,Q) :-
	append("Positive part: ", L, String),
	self(AspInterface),
	Q new wellfound(AspInterface),
	parse1(L,Q,A),
	negative_part(OSM,A,Q),!.
negative_part(OSM,A,Q) :- 
	get_line(OSM,String1),
	append("Negative part: ", L1, String1),
	parse1(L1,Q,A),!.

negative_part(_,_,_).
:- data qualify/2, counter/1, tmpFile/1, tmpST/1.
:- data weight/2, const/2.

existgroundASP :-
	aspFileName(ASPFile),
	atom_concat(ASPFile, '1',ASPF),
	file_exists(ASPF),!.

groundAsp :-
	retractall_fact(qualify(_,_)),
	retractall_fact(weight(_,_)),
	retractall_fact(const(_,_)),
       retractall_fact(tmpST(_)),
	aspFileName(ASPFile),
	children(L),
	(L=[]->fail;atom_concat(ASPFile,'1',ASPTmp)),!,
       ST new symboltab,
       assertz_fact(tmpST(ST)),
	open(ASPFile, read, ASPRead),
	open(ASPTmp, write, Tmp),
	saveTmp(Tmp),
	read_file(ASPRead, LineTerm, V),
	factorRule(Tmp,LineTerm, V),!,
	close(ASPRead), close(Tmp), 
        !.
groundAsp.

factorRule(_,end_of_file, _V) :- !.
factorRule(_,end, _V) :- !.
factorRule(_,'#use_asp'(_,_),_V) :-
	!,fail.

factorRule(_,'#use_asp'(_,_,_),_V) :-
	!,fail.

factorRule(_,'#import'(_,_),_V) :-
	!,fail.

factorRule(_, '#const'(=(C,V)), _V) :- !,
	assertz_fact(const(C,V)),
	getTmp(Tmp),
	write(Tmp, '#const '),
	write(Tmp, C),
	write(Tmp, '='),
	write(Tmp, V),
	write(Tmp, '.'), nl(Tmp), !, fail.
factorRule(_, '#weight'(=(W,V)), true) :- !,
	assertz_fact(weight(W,V)),
	getTmp(Tmp),
	write(Tmp, '#weight '),
	write(Tmp, W),
	write(Tmp, '='),
	write(Tmp, V),
	write(Tmp, '.'), nl(Tmp), !, fail.
factorRule(_, '#show'(A), true) :- !,
	getTmp(Tmp),
	write(Tmp, '#show '),
	write(Tmp, A),
	write(Tmp, '.'), nl(Tmp), !, fail.
factorRule(_, '#hide'(A), true) :- !,
	getTmp(Tmp),
	write(Tmp, '#hide '),
	write(Tmp, A),
	write(Tmp, '.'), nl(Tmp), !, fail.
factorRule(Tmp,LineTerm,V) :-
	LineTerm =.. RL,
	RL=[':-',Head,Body],!,
	!,groundRule(Tmp,LineTerm,Head,Body,V),!,fail.

factorRule(Tmp,LineTerm,V) :-
	addNewRule(Tmp,LineTerm,V),!,fail.

groundRule(Tmp,LineTerm,Head,Body,V) :-
	reset_counter,
	groundPred1(LineTerm,Body,Body1),
	groundPred2(LineTerm,Body1,Body2,V),
	(Body2=true -> RL=Head;RL=..[':-',Head,Body2]),
	addNewRule(Tmp,RL,V),fail.

groundPred1(R, ((A:model(B)),Rest), Rest1) :- !,
	(ground(A) -> true;grounderr(1)),
	children(ChildModules),
	memberChild(A,_,ChildModules,1),!,
	term_to_string(B,StrB),
	saveQualification(A,StrB),
	groundPred1(R,Rest,Rest1).
groundPred1(R,((#(Up,@(Low,A))),Rest),Rest1):- !,
	groundPred1(R,A,T1),
	(T1=true -> grounderr(1);A1=(#(Up,(@(Low,T1))))),
	groundPred1(R,Rest,T2),
	(T2=true -> Rest1=A1;Rest1=(A1,T2)).
groundPred1(R,((#(@(Low,A))),Rest),Rest1):- !,
	groundPred1(R,A,T1),
	(T1=true -> grounderr(1);A1=(#(@(Low,T1)))),
	groundPred1(R,Rest,T2),
	(T2=true -> Rest1=A1;Rest1=(A1,T2)).
groundPred1(R,((#(Up,@(A))),Rest),Rest1):- !,
	groundPred1(R,A,T1),
	(T1=true -> grounderr(1);A1=(#(Up,(@(T1))))),
	groundPred1(R,Rest,T2),
	(T2=true -> Rest1=A1;Rest1=(A1,T2)).
groundPred1(R,((#(@(A))),Rest),Rest1):- !,
	groundPred1(R,A,T1),
	(T1=true -> grounderr(1);A1=(#(@(T1)))),
	groundPred1(R,Rest,T2),
	(T2=true -> Rest1=A1;Rest1=(A1,T2)).
groundPred1(R,((&(Up,^(Low,A))),Rest),Rest1):- !,
	groundPred1(R,A,T1),
	(T1=true -> grounderr(1);A1=(&(Up,(^(Low,T1))))),
	groundPred1(R,Rest,T2),
	(T2=true -> Rest1=A1;Rest1=(A1,T2)).
groundPred1(_R,(=(:(_A,model(_B)),_N),_Rest), _Rest1) :- !,
	grounderr(1).
groundPred1(_, (not(_:model(_)),_), _) :- !,
	grounderr(1).
groundPred1(R,(A,Rest),Rest1) :- !,
	groundPred1(R,Rest,T),
	(T=true-> Rest1=A;Rest1=(A,T)).
groundPred1(_,(A:model(B)),true) :- !,
	(ground(A) -> true;grounderr(1)),
	children(ChildModules),
	memberChild(A,_,ChildModules,1),!,
	term_to_string(B,StrB), 
	saveQualification(A,StrB).
groundPred1(R,(#(Up,@(Low,A))),A1):-
	groundPred1(R,A,T1),
	(T1=true -> grounderr(1);A1=(#(Up,(@(Low,T1))))).
groundPred1(R,(&(Up,^(Low,A))),A1):-
	groundPred1(R,A,T1),
	(T1=true -> grounderr(1);A1=(&(Up,(^(Low,T1))))).
groundPred1(_R,(=(:(_A,model(_B)),_N)), _Rest1) :- !,
	grounderr(1).
groundPred1(_, (not(_:model(_))), _) :- !,
	grounderr(1).
groundPred1(_,A,A).

groundPred2(R, ((Q:B),Rest), Rest1,V) :- 
	B =.. [_|ArgList],
	(var(Q) -> Arg=[Q|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(Q,B,T,V),
	!, groundPred2(R,Rest,Rest2,_V),
	(Rest2=true -> Rest1=T;Rest1=(T,Rest2)).
groundPred2(R,((#(Up,@(Low,A))),Rest),Rest1,V):-
	groundPred2(R,A,T1,V),
	(T1=true -> A1=true;
	(termDiff(A,T1,D),L1 is Low-D,U1 is Up-D,A1=(#(U1,(@(L1,T1)))))),
	groundPred2(R,Rest,T2,_V1),
	(T2=true -> Rest1=A1;(A1=true->Rest1=T2;Rest1=(A1,T2))).
groundPred2(R,((&(Up,^(Low,A))),Rest),Rest1,V):-
	groundPred2(R,A,T1,V),
	(T1=true -> A1=true;
	(termDiff(A,T1,D),L1 is Low-D,U1 is Up-D,
	A1=(&(U1,(^(L1,T1)))))),
	groundPred2(R,Rest,T2,_V1),
	(T2=true -> Rest1=A1;(A1=true->Rest1=T2;Rest1=(A1,T2))).
groundPred2(R,(=(:(A,B),N),Rest),Rest1,V) :- !,
	B =.. [_|ArgList],
	(var(A) -> Arg=[A|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(A,B,T,V),
	!,groundPred2(R,Rest,Rest2,V),
	(Rest2=true -> Rest1=(=(T,N));Rest1=(=(T,N),Rest2)).
groundPred2(R,(not(=(:(A,B),N)),Rest),Rest1,V) :- !,
	N1 is -N,
	groundPred2(R,(=(:(A,B),N1),Rest), Rest1, V).
groundPred2(R,(:(:(A,B),C), Rest),Rest1,V) :- !,
	B =.. [_|ArgList],
	(var(A) -> Arg=[A|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(A,B,T,V),
	!, groundPred2(R,Rest,Rest2,V),
	(Rest2=true -> Rest1=(:(T,C));Rest1=(:(T,C),Rest2)).
groundPred2(R,(not(A:B),Rest), Rest1,V) :-
	B =.. [_|ArgList],
	(var(A) -> Arg=[A|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(A,B,T,V),
	!,groundPred2(R,Rest,Rest2,V),
	(Rest2=true -> Rest1=not(T);Rest1=(not(T),Rest2)).
groundPred2(R,(H,Rest),Rest1,_V) :- !,
	groundPred2(R,Rest,T,_V), 
	(T=true-> Rest1=H;Rest1=(H,T)).
groundPred2(_R,(Q:B),T,V) :- 
	B =.. [_|ArgList],
	(var(Q) -> Arg=[Q|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(Q,B,T,V),!.
groundPred2(R,(#(Up,@(Low,A))),A1,V):-
	groundPred2(R,A,T1,V),
	(T1=true -> A1=true;
	(termDiff(A,T1,D),L1 is Low-D,U1 is Up-D,
	A1=(#(U1,(@(L1,T1)))))).
groundPred2(R,(&(Up,^(Low,A))),A1,V):-
	groundPred2(R,A,T1,V),
	(T1=true -> A1=true;
	(termDiff(A,T1,D),L1 is Low-D,U1 is Up-D,
	A1=(&(U1,(^(L1,T1)))))).
groundPred2(_R,(=(:(A,B),N)),(=(T,N)),V) :- !,
	B =.. [_|ArgList],
	(var(A) -> Arg=[A|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(A,B,T,V),
	!.
groundPred2(R,(not(=(:(A,B),N))),Rest1,V) :-
	N1 is -N,
	groundPred2(R,(=(:(A,B),N1)), Rest1,V).
groundPred2(_R,(:(:(A,B),C)),(:(T,C)),V) :- !,
	B =.. [_|ArgList],
	(var(Q) -> Arg=[Q|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(A,B,T,V),
	!.
groundPred2(_R,(not(A:B)), (not(T)),V) :-
	B =.. [_|ArgList],
	(var(A) -> Arg=[A|ArgList];Arg=ArgList),
	tmpVarTerm(_,Arg,T),
	groundTempVar(A,B,T,V),!.
groundPred2(_R,A,A,_V).

groundTempVar(A,B,T,V) :-
	groundPred3(T,(A:B)), 
	getTmp(Tmp),
	addNewRule(Tmp,T,V),fail.
groundTempVar(_,_,_,_V).

groundPred3(R, (Q:B)) :- 
	(var(Q) -> (term_to_string(Q,StrQ),qualify(A,StrQ));A=Q),
	children(ChildModules),
	memberChild(A,F,ChildModules,Type),!,
	!,groundOnePred(R,Q,F,B,Type).
memberChild(A,F,ChildList,0) :- member(prolog(A,F),ChildList),!.
memberChild(A,F,ChildList,1) :- member(asp(A,F),ChildList).

groundOnePred(R,IntName,ModuleName,B,0) :-
	B =.. [Pred|ArgList],
	findall(B,ModuleName:B,L),
	list2set(L,L1),
	groundList(R,Pred,ArgList,L1),
	functor(R,PRED,N),
	tmpST(ST),
	(ST:symbolTable(IntName,Pred,PRED,N,0) -> true;
	ST:addSymbolTable(IntName,Pred,PRED,N,0)).
groundOnePred(R,QC,ModuleName,B,1) :-
	var(QC),!,
	module_concat(ModuleName,model(Q),AA),
	'$meta_call'(AA),
	Q:getTrueAtoms(L),
	B =.. [Pred|ArgList],
	groundList(R,Pred,ArgList,L),
	module_concat(ModuleName,aspModel(_,ModelNo,Q),G1),
	'$meta_call'(G1),
	R =.. [_|Arg2],
	Arg2=[ModelNo|_],
	functor(R,PRED,ArgNo),
	tmpST(ST),
	(ST:symbolTable(aspm(Q,ModelNo,ModuleName),Pred,PRED,ArgNo,1) ->true;
	ST:addSymbolTable(aspm(Q,ModelNo,ModuleName),Pred,PRED,ArgNo,1)).
groundOnePred(R,_IntName,ModuleName,B,1) :- !,
	module_concat(ModuleName,getAllSkepTrue(L),AA),
	'$meta_call'(AA),
	B =.. [Pred|ArgList],
	groundList(R,Pred,ArgList,L),
	functor(R,PRED,N),
	tmpST(ST),
	module_concat(ModuleName,stateSkep(_,SK),G1),
	'$meta_call'(G1),
	(ST:symbolTable(aspm(SK,0,ModuleName),Pred,PRED,N,1) -> true;
	ST:addSymbolTable(aspm(SK,0,ModuleName),Pred,PRED,N,1)).

groundList(R,Pred,ArgList,[H|_]) :-
	H =.. [Pred|Ta],
	start_ground(ArgList, Ta, R).
groundList(R,Pred,ArgList,[_|T]) :-
	groundList(R,Pred,ArgList,T).
start_ground([H|T],[H1|T1],R) :-
	ground(H), !,
	H=H1,
	start_ground(T,T1,R).

start_ground([H|T],[H1|T1],R) :-
	var(H),!,
	H=H1,
	start_ground(T,T1,R).
start_ground([], [], _).

addNewRule(Tmp, R,V) :-
	change_names(V),
	reverseFormat(R,Str),
	retractall_fact(ruleString(_)),
	write_string(Tmp,Str), write(Tmp,'.'), nl(Tmp),!.

deleteGroundASP :-
	aspFileName(ASPFile),
	atom_concat(ASPFile,'1',ASPTmp),
	(file_exists(ASPTmp)->delete_file(ASPTmp);true).

incCounter :- counter(N), !,N1 is N+1,
	retractall_fact(counter(_)),
	assertz_fact(counter(N1)).
incCounter :- reset_counter.
reset_counter :- retractall_fact(counter(_)),
	assertz_fact(counter(1)).

grounderr(1) :- 
	this_module(M),
	aspFileName(AspF),
	Err='grounding asp module: ',
	atom_concat(Err, AspF, Err1),
	atom_concat(Err1,' at line: ', Err2),
	lineNoAtom(_,ANo),
	atom_concat(Err2, ANo, Err3),
	err(0,M,Err3,0).
prtSymbolTable :- 
	tmpST(ST), ST:prtSymbolTable.

saveQualificatoin(A,B) :- qualify(A,B),!.
saveQualification(A,B) :- assertz_fact(qualify(A,B)).

saveTmp(Tmp) :- retractall_fact(tmpFile(_)), 
	assertz_fact(tmpFile(Tmp)).
getTmp(Tmp) :- tmpFile(Tmp).

:- data stateAtom/2.

currentAtom(State, A) :-
	stateAtom(State, A),!.
currentAtom(State, A) :-
       aspFileName(AspF),
	children(L), (L=[]->F=AspF;atom_concat(AspF,'1',F)),
	atom_concat('lparse -d all ',F,EXELP),
	popen(EXELP,read,LP),
	skip_lines(LP,State,A),!.
skip_lines(LP,State,A) :- 
	repeat,
        get_line(LP, String),
	String="0",
        read_pred(LP, State, A), !.

read_pred(LP, State, A) :- 
	A new atomclass,
        repeat,
        get_line(LP, String),
        process_lparse(A, String),!,
	asserta_fact(stateAtom(State,A)),
	A:incRef.
process_lparse(_,"0") :- !. 
process_lparse(A,String) :- 
       pos(" ", String, L1, L2),
	string2term(L1,A1),
       string2term(L2, A2),
        A:addAtom(A1, A2), !,fail.

:- data stateSkep/2, ans/2.

:- export(getSkep/2).
:- export(getAllSkepTrue/1).

getAllSkepTrue(L) :-
	var(L),
	retractall_fact(ans(_,_)),
	getCState(State),
	(stateSkep(State,Sk) -> true;
	skep(State,Sk)),
	Sk:getAllSkepTrue(L),
	retractall_fact(ans(_,_)).

getSkep(Atom,V) :-
	retractall_fact(ans(_,_)),
	getCState(State),
	(stateSkep(State,Sk)-> true;
	skep(State,Sk)),
	 Sk:getSkep(Atom,V),
	retractall_fact(ans(_,_)).

skep(State,Sk) :-
	computeSkep(State,Sk),
	Sk:saveState(State).

computeSkep(_,_) :- 
	retractall_fact(ans(_,_)),
	self(ModuleName),
	module_concat(ModuleName,model(Q),G),
	'$meta_call'(G),
	Q:getTrueAtoms(TAtoms),
	Q:getFalseAtoms(FAtoms),
	saveSkep(TAtoms,FAtoms),
	fail.
computeSkep(State,Sk) :- 
	ans(TAtoms,FAtoms),
	Sk new skepclass,
	stateAtom(State,A),
	assertz_fact(stateSkep(State,Sk)),
	Sk:addClasses(A),
	(tmpST(ST)->(Sk:data_add(symboltableClass(ST)),
	ST:incRef);true),
	Sk:addSkep(TAtoms,1),
	Sk:addSkep(FAtoms,0).
saveSkep(NewTAtoms,NewFAtoms) :-
	ans(TAtoms,FAtoms),!,
	intersection(TAtoms,NewTAtoms,TIntersect),
	intersection(FAtoms,NewFAtoms,FIntersect),
	retractall_fact(ans(_,_)),
	assertz_fact(ans(TIntersect,FIntersect)),!.
saveSkep(NewTAtoms,NewFAtoms) :-
	assertz_fact(ans(NewTAtoms,NewFAtoms)).

:- data curr_state/1, state/1, stateChanged/0.
curr_state(0).
%------------------------------------------------
% curr_state handler...
%-----------------------
inc_state :- curr_state(State),
	state_exists(State,State1),
       retractall(state(_)),
	deleteGroundASP,
	push_state(State1).

dec_state :- curr_state(N), N>0, !,
	retract_fact(curr_state(N)),
	deleteGroundASP,
	remState(N),
	retractall_fact(state(_)).
dec_state :- curr_state(0), !, inc_state.
dec_state :- assertz_fact(state(0)).

getCState(State) :- curr_state(State).


get_state(State) :- curr_state(State),!.
get_state(State) :- var(State), !,
	state_exists(0,State),
	set_curr_state(State),curr_state(State).

push_state(State) :- curr_state(State),!.
push_state(State) :- asserta_fact(curr_state(State)).

pop_state :- curr_state(State),
       remState(State),
       State>0, !,
	retract_fact(curr_state(State)).

state_exists(State,State1) :-
       aspModelExist(State),
	State2 is State+1, !,
	state_exists(State2,State1).
state_exists(State,State).

set_curr_state(_) :- retractall_fact(curr_state(_)), fail.
set_curr_state(N) :- assertz_fact(curr_state(N)).

rem_curr_state :- retractall_fact(curr_state(_)), !.
rem_curr_state.

%------------------------------------------------

incParentState :-
	parent(L),
	stateChanged,!,
	incEachParentState(L),
	retractall_fact(stateChanged).
incParentState.

incEachParentState([]).
incEachParentState([H|T]) :-
	atom_concat(H, ':', H1),
	atom_concat(H1,'setStateChanged',ChangeState),
	atom_concat(H1, 'incState', IncState),
	'$meta_call'(ChangeState),
	'$meta_call'(IncState),
	incEachParentState(T).

decParentState :-
	parent(L),
	stateChanged,!,
	decEachParentState(L),
	retractall_fact(stateChanged).
decParentState.

decEachParentState([]).
decEachParentState([H|T]) :-
	atom_concat(H, ':', H1),
	atom_concat(H1,'setStateChanged',ChangeState),
	atom_concat(H1, 'dec_state', DecState),
	'$meta_call'(ChangeState),
	'$meta_call'(DecState),
	decEachParentState(T).

incState :-
	stateChanged,!,
	inc_state, incParentState,
	self(ModuleName),
	module_concat(ModuleName,resetStateChanged,G),
	'$meta_call'(G).
incState.

decState :-
	stateChanged,!,
	dec_state, decParentState,
	self(ModuleName),
	module_concat(ModuleName,resetStateChanged,G),
	'$meta_call'(G).
decState.

:- export(setStateChanged/0).
:- export(resetStateChanged/0).
setStateChanged :- stateChanged,!.
setStateChanged :- assertz_fact(stateChanged).

resetStateChanged :- stateChanged,!,
	retractall_fact(stateChanged).
resetStateChanged.

% pos: determines if first parameter string in the second parameter string,
% if so, then third parameter is the string before and the last parameter
% is the string after. e.g. pos("omar", "hi omar nabil", S1, S2), then
% S1="hi " and S2=" nabil".xs
pos([H1|T1], [H1|T2], [], T) :- 
	check_true_string(T1, T2, T), !.
pos(L, [H2|T2], [H2|T3], A) :- pos(L, T2, T3, A).

check_true_string([], T, T).
check_true_string([H1|T1], [H1|T2], T) :- 
        check_true_string(T1, T2, T).
% --------------------------------------------------------------

cmp :- aspFileName(X),
       modif_time(X,T),
       mftime(T).

set_ftime :- aspFileName(X),
       modif_time(X,T),
       rem_mftime,
       assertz(mftime(T)).

rem_mftime :- retractall(mftime(_)),!.
rem_mftime.


% concat a list of atoms all together
atoms_concat([], '') :- !.
atoms_concat([[]|T], C) :- 
       atoms_concat(T,C),!.
atoms_concat([H|T], C) :-
	atoms_concat(T, C1),
	atom_concat(H, C1, C).

% append to file new rules in case of assert.
append_to_file(_, []).
append_to_file(S, [H|T]) :-
	makeV(1,V),
	assignV(H,V),
   reverseFormat(H, String),
   write_string(S, String),
   write(S, '.'),
   nl(S),
   append_to_file(S, T).
% -------------------------------------------
cnv_atom(N, Atom) :- number(N), !, number_atom(N,Atom).
cnv_atom(N, Atom) :- string(N), !, atom_codes(Atom, N).
cnv_atom(A, A).

cnv2atom_list([],[]).
cnv2atom_list(String,Atom) :-
	get_each_atom(String, String1, Rest),
	name(Atom1, String1),
	cnv2atom_list(Rest,Atom2),
	append([Atom1],Atom2,Atom).

get_each_atom(String, String1, Rest) :-
    pos(" ", String, String1, Rest),!.
get_each_atom(String,String,[]).

% append atom_list1 to atom_list2 that are not computed.
my_append([], A, A).
my_append([H|T], A, L) :- computed(H), !, my_append(T, A, L).
my_append([H|T], A, L) :- member(H, A), !, my_append(T, A, L).
my_append([H|T], A, [H|T1]) :- my_append(T, A, T1).

%change number to atom
number_atom(N, A) :- 
	number_codes(N, S),
	atom_codes(A, S).

% writting comma
wrt_comma(0, _, _).
wrt_comma(1, S1, S2) :- write(S1, ','), write(S2, ',').

prt_comma(0,'').
prt_comma(1,',').


del_file(X) :- file_exists(X), delete_file(X), !.
del_file(_).

% predicate will work on backtrack. --------
und(X) :- '$undo_goal'(call(X)).
% ------------------------------------------

disp_list([]).
disp_list([H|T]) :- var(H), !,display(H), disp_list(T).
disp_list([nl|T]) :- !, nl, disp_list(T).
disp_list([H|T]) :- string(H), !, write_string(H), disp_list(T).
disp_list([H|T]) :- display(H), disp_list(T).

empty_list([]) :- !, fail.
empty_list(_).

put_comma(_, 1).
put_comma(S, A) :- A>1, write(S, ',').

writeList2File(_,[],_).
writeList2File(S,[H|T],C) :- 
	put_comma(S,C),
	write(S,H),
	writeList2File(S,T,1).

list2set([],[]).
list2set([H|T],R) :- member(H,T), !,list2set(T,R).
list2set([H|T],[H|R]) :- list2set(T,R).

change_names([]).
change_names([H=H1|T]) :-
	var(H1),!,
	H=H1,
	change_names(T).
change_names([_|T]) :- change_names(T).

:- data varNames/1.

% string-->term and term-->string.
strToTerm(S,T,V) :-
        mktemp('t2sXXXXXX',TMP),
        open_output(TMP,Out),
        write_string(S), 
%	write('.'),
        close_output(Out),
        open_input(TMP, In),
        read_term(T,[variable_names(V)]),
        close_input(In),
        delete_file(TMP).

% string-->term and term-->string.
string_to_term(S,T) :-
        mktemp('t2sXXXXXX',TMP),
        open_output(TMP,Out),
        write_string(S), 
%	write('.'),
        close_output(Out),
        open_input(TMP, In),
        read_term(T,[variable_names(_V)]),
        close_input(In), 
        delete_file(TMP).

writeTerm(T) :- var(T), !, write(T).
writeTerm(:-(Head,Body)) :-
	writeTerm(Head),
	write(' :- '),
	writeTerm(Body).
writeTerm((#(^(N,A)),Rest)) :-
	number(N),
	 write(N),write(' { '),write(A),
	write(' } '),
	write(','),
	writeTerm(Rest).
writeTerm((#(U,^(A)),Rest)) :-
	number(U),
	write(' { '),write(A),
	write(' } '),
	write(U),
	write(','),
	writeTerm(Rest).
writeTerm((#(U,^(N,A)),Rest)) :-
	number(N),
	 write(N),write(' { '),write(A),
	write(' } '),
	write(U),
	write(','),
	writeTerm(Rest).
writeTerm(#(^(N,A))) :-
	number(N),
	write(N),write(' { '),write(A),
	write(' } ').
writeTerm((#(U,^(A)))) :-
	number(U),
	write(' { '),write(A),
	write(' } '),
	write(U).
writeTerm((#(U,^(N,A)))) :-
	number(N),
	 write(N),write(' { '),write(A),
	write(' } '),
	write(U).
writeTerm((@(&(N,A)),Rest)) :-
	number(N),
	 write(N),write(' [ '),write(A),
	write(' ] '),
	write(','),
	writeTerm(Rest).
writeTerm((@(&(A),U),Rest)) :-
	number(U),
	write(' [ '),write(A),
	write(' ] '),
	write(U),
	write(','),
	writeTerm(Rest).
writeTerm((@(&(N,A),U),Rest)) :-
	number(N),
	 write(N),write(' [ '),write(A),
	write(' ] '),
	write(U),
	write(','),
	writeTerm(Rest).
writeTerm(@(&(N,A))) :-
	number(N),
	write(N),write(' [ '),write(A),
	write(' ] ').
writeTerm(@(&(A),U)) :-
	number(U),
	write(' [ '),write(A),
	write(' ] '),
	write(U).
writeTerm((@(&(N,A),U))) :-
	number(N),
	 write(N),write(' [ '),write(A),
	write(' ] '),
	write(U).
writeTerm(T) :-
	write(T).

term_to_string(T,S) :-
        mktemp('t2sXXXXXX', TMP), 
        open_output(TMP,Out),
        writeTerm(T),
        close_output(Out),
        open_input(TMP, In),
        get_line(S),
        close_input(In), 
        delete_file(TMP).

termToStr(T,S) :-
        mktemp('t2sXXXXXX', TMP), 
        open_output(TMP,Out),
        write_term(T,[quoted(true)]),
        close_output(Out),
        open_input(TMP, In),
        get_line(S),
        close_input(In), 
        delete_file(TMP).
%----------------------------------------

write2ASPFile(S,end_of_file,_V) :- emptyRuleString(S).
write2ASPFile(_,end,_V).
write2ASPFile(S,'#const'(X),_V) :-
	emptyRuleString(S),
	write(S,'#const '),
	write(S,X),
	!,fail.
write2ASPFile(S,'#use_asp'(A,B,C),_V) :- !, 
	emptyRuleString(S),
	write(S,'#use_asp('),
	write_term(S,A,[quoted(true)]),
	write(S,','),
	write_term(S,B,[quoted(true)]),
	write(S,','),
	write_term(S,C,[quoted(true)]),
	write(S,').'), nl(S),!, fail.
write2ASPFile(S,'#use_asp'(A,B),_V) :- !,
	emptyRuleString(S),
	write(S,'#use_asp('),
	write_term(S,A,[quoted(true)]),
	write(S,','),
	write_term(S,B,[quoted(true)]),
	write(S,').'), nl(S),!, fail.
write2ASPFile(S,'#import'(A,B),_V) :- !,
	emptyRuleString(S),
	write(S,'#import('),
	write_term(S,A,[quoted(true)]),
	write(S,','),
	write_term(S,B,[quoted(true)]),
	write(S,').'), nl(S),!, fail.
write2ASPFile(Tmp,AtmTerm,V) :-
	emptyRuleString(Tmp),
	change_names(V),
	reverseFormat(AtmTerm,Str),
	write_string(Tmp,Str),
       write(Tmp, '.'),nl(Tmp),
	!,fail.
emptyRuleString(Tmp) :-
	ruleString(String),
	write_string(Tmp,String), 
	nl(Tmp), fail.
emptyRuleString(_) :- retractall_fact(ruleString(_)).

lineNoAtom(N,ANo) :-
	lineNo(N),
	number_atom(N,ANo).
termDiff(T1,T2,Diff) :- 
	termCount(T1,D1),
	termCount(T2,D2),
	Diff is D1-D2.
termCount('',0).
termCount((_=W,B),D) :- !,
	(number(W) -> V=W;const(W,V)),
	(number(V)-> V1=V;weight(V,V1)),
	termCount(B,D1),
	D is D1+V1.
termCount((_,B),D) :- !,
	termCount(B,D1),
	D is D1+1.

% reset_asp : deletes all stored models.
%             initialize: curr_state(0).
reset_asp :- set_curr_state(0),
    remove_all_classes,
	deleteGroundASP,
	retractall_fact(endModels(_)).
remove_all_classes :- 
	retractall_fact(aspModel(_,_,_)),
	retractall_fact(wfmState(_,_)),
	retractall_fact(stateSkep(_,_)),
	retractall_fact(stateAtom(_,_)).

remState(State) :- 
	remStateClasses(State),
	retractall_fact(endModels(State)).

remStateClasses(St) :- 
	retractall_fact(aspModel(St,_,_)),
	retractall_fact(wfmState(St,_)),
	retractall_fact(stateSkep(St,_)),
	retractall_fact(stateAtom(St,_)).

% release a model name ASP_Name....
release(Q) :-
	remAspModel(Q), !,
	destroy(Q),!.
release(_).
% -----------------------------------------
remAspModel(Q) :- retract_fact(aspModel(_,_,Q)),!.
remAspModel(_).

cnv2NProlog(ASPFormat, NPFormat) :-
	string(ASPFormat), !,
	stringConvert2EP(ASPFormat, NPFormat).

cnv2NProlog(ASPFormat, NPFormat) :-
	list(ASPFormat),!,
	cnvList2NPList(ASPFormat, NPFormat).
cnv2NProlog(ASPFormat, NPFormat) :-
	(atom(ASPFormat);term(ASPFormat)),!,
	atomTerm2NPAtomTerm(ASPFormat, NPFormat).
cnvList2NPList([], []).
cnvList2NPList([H|T], [H1|T1]) :-
	((atom(H);term(H))-> 	 atomTerm2NPAtomTerm(H,H1);	stringConvert2EP(H,H1)),
	 cnvList2NPList(T,T1).
atomTerm2NPAtomTerm(A, B) :-
	(atom(A) -> name(A, Str1); term_to_string(A,Str1)),
	stringConvert2EP(Str1, Str2),
	 append(Str2, ".", Str4),
	 string_to_term(Str4,B),!.

% convert special: '{' --> '^', '}' --> '#', '[' --> '&', ']' --> '@'
stringConvert2EP(String,String) :-
	append("#const ", _, String).
stringConvert2EP(String,String) :-
	append("#domain ", _, String).
stringConvert2EP(String,String) :-
	append("#external ", _, String).
stringConvert2EP(String,String) :-
	append("#function ", _, String).
stringConvert2EP(String,String) :-
	append("#hide ", _, String).
stringConvert2EP(String,String) :-
	append("#show ", _, String).
stringConvert2EP(String,String) :-
	append("#weight ", _, String).
stringConvert2EP(String,String) :-
	append("#option ", _, String).
stringConvert2EP([], []).
stringConvert2EP([0'{|T], [0'^|T1]) :- !,
	stringConvert2EP(T, T1).
stringConvert2EP([0'}|T], [0'#|T1]) :- !,
	stringConvert2EP(T, T1).
stringConvert2EP([0'[|T], [0'&|T1]) :- !,
	stringConvert2EP(T, T1).
stringConvert2EP([0']|T], [0'@|T1]) :- !,
	stringConvert2EP(T, T1).
stringConvert2EP([H|T], [H|T1]) :-
	 stringConvert2EP(T, T1).
%-------------------------------------------------
% reverse format '{' --> '^', '}' --> '#', '[' --> '&', ']' --> '@'.
reverseFormat(Atm, String) :-
   atom(Atm), 
   name(Atm, String1),
   rem_last_dot(String1, String2),
   reverse_convert(String2, String).
reverseFormat(Term, String) :-
      term(Term), !,
      term_to_string(Term, String1),
      reverse_convert(String1, String).
reverse_convert([], []).
reverse_convert(L1,L1) :-
	append("#use_asp",_,L1),!.
reverse_convert(L1,L1) :-
	append("#import",_,L1),!.
reverse_convert(L1,L1) :-
	append("#const",_,L1),!.
reverse_convert(L1,L1) :-
	append("#weight",_,L1),!.
reverse_convert(L1,L1) :-
	append("#domain",_,L1),!.
reverse_convert(L1,L1) :-
	append("#external",_,L1),!.
reverse_convert(L1,L1) :-
	append("#function",_,L1),!.
reverse_convert(L1,L1) :-
	append("#hide",_,L1),!.
reverse_convert(L1,L1) :-
	append("#show",_,L1),!.
reverse_convert(L1,L1) :-
	append("#option",_,L1),!.
reverse_convert([0'^|T1], [0'{|T2]) :- !,
   reverse_convert(T1,T2).
reverse_convert([0'#|T1], [0'}|T2]) :- !,
   reverse_convert(T1,T2).
reverse_convert([0'&|T1], [0'[|T2]) :- !,
   reverse_convert(T1,T2).
reverse_convert([0'@|T1], [0']|T2]) :- !,
   reverse_convert(T1,T2).
reverse_convert([H|T],[H|T1]) :- 
	var(H),
	reverse_convert(T,T1).
reverse_convert([0'~|T], [0'"|T1]) :- !,
	reverse_convert(T,T1).
reverse_convert([H1|T1], [H1|T2]) :-
   !, reverse_convert(T1,T2).

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


:- data debug_flag/1, pred_flag/1.
dmsg(dLevel,Msg) :-
    list(Msg),
    debug_flag(dLevel),
    disp_list(Msg),!.
dmsg(dLevel,Msg) :-
    debug_flag(dLevel),
    display(Msg),!.
dmsg(_,_).

dcall(dLevel,Pred) :-
    pred_flag(dLevel),
    call(Pred),!.
dcall(_,_).

:- data error/3.
err(_,M,Action,0) :-
	display('Module '), display(M),display(': '),
	display('Error in '), display(Action), nl, 
	abortExec(0).
err(error(syntax_error(_,_,_,L),Pred),M,Action,1) :-
	display('Module '), display(M),display(:),
	display('Error in '), display(Pred), nl,
	display(Action),nl,
	display('Entred term is:'),
	atomList2string(L,Str),
	name(A,Str),
	display(A),!, nl.

atomList2string([],"").
atomList2string(['
'|T],S) :- !,
	atomList2string(T,S).
atomList2string(['** here **'|T],S) :- !,
	atomList2string(T,S).
atomList2string([H|T],S) :-
	name(H,A),
	atomList2string(T,S1),
	append(A,S1,S).

err1(Stop, Msg, Pred) :-
	dispError(Stop),
	disp_list([Msg, ' in Pred:', Pred]), nl,
	abortExec(Stop).

err2 :- 
	error(Stop,Msg,Pred),
	dispError(Stop),
	disp_list([Msg, ' in Pred:', Pred]), nl,
	abortExec(Stop).

dispError(0) :- display('Error:').
dispError(1) :- display('Warning:').

abortExec(0) :- 
	display('abort Execution.'),nl,nl,abort.
abortExec(_).

:- data varc/1.

setVar(N) :- retractall_fact(varc(_)),
	assertz_fact(varc(N)).

incVar(N) :- var(N),
	(varc(N) -> true;N=0),
	N1 is N+1,
	setVar(N1).

getVar(TmpVar) :- var(TmpVar),
	incVar(N),
	cnv_atom(N,A),
	atom_concat('aspprolog',A,TmpVar).

tmpVarTerm(TmpVarName,ArgList,Term) :- 
	getVar(TmpVarName),
	Term =.. [TmpVarName|ArgList].

