:- module(misc, []).

:- use_module(library(streams)).
:- use_module(library(strings)).
:- use_module(library(filenames)).

:- export(cnv2atom_list/2).
:- export(cnv_atom/2).
:- export(atoms_concat/2).
:- export(number_atom/2).
%:- export(save_parm/3).
:- export(put_comma/2).
:- export(chk_file_exist/1).
:- export(must_exist/1).
:- export(pos/4).
:- export(pos2/4).
:- export(term_to_string/2).
:- export(string_to_term/2).
:- export(empty_list/1).
:- export(disp_list/1).

cnv2atom_list([],[]).
cnv2atom_list(String,Atom) :-
	get_each_atom(String, String1, Rest),
	name(Atom1, String1),
	cnv2atom_list(Rest,Atom2),
	append([Atom1],Atom2,Atom).

get_each_atom(String, String1, Rest) :-
    pos(" ", String, String1, Rest),!.
get_each_atom(String,String,[]).

cnv_atom(N, Atom) :- number(N), !, number_atom(N,Atom).
cnv_atom(N, Atom) :- string(N), !, atom_codes(Atom, N).
cnv_atom(A, A).

% concat a list of atoms all together
atoms_concat([], '') :- !.
atoms_concat([[]|T], C) :- 
       atoms_concat(T,C),!.
atoms_concat([H|T], C) :-
	atoms_concat(T, C1),
	atom_concat(H, C1, C).
% -----------------------------------------------------------------

number_atom(N, A) :- 
    number_codes(N, S),
    atom_codes(A, S).

put_comma(_, 1).
put_comma(S, A) :- A>1, write(S, ',').

% -------------------------------------------
% Predicate handle files existance:

chk_file_exist(A) :-
        file_exists(A),
        display('File '), display(A), display(' exist, overwrite (y/n)?'),
        get1_code(AN),
        chk_ans(AN), !.
chk_file_exist(_).


chk_ans(X) :- y_char(X), !.
chk_ans(_) :- !, abort.

y_char(X) :- X=89 ; X=121.

must_exist(A) :- file_exists(A), !.
must_exist(A) :- display('File '), display(A), display(' does not exist!!! '),
        display('Execution aboreted'), nl,
        !, abort.
% -----------------------------------------------------------

pos([H1|T1], [H1|T2], [], T) :- 
        check_true_string(T1, T2, T), !.
pos(L, [H2|T2], [H2|T3], A) :- pos(L, T2, T3, A).

check_true_string([], T, T).
check_true_string([H1|T1], [H1|T2], T) :- 
        check_true_string(T1, T2, T).

pos2(_, [], [], []).
pos2([H1|T1], [H1|T2], [], T) :- 
        check_true_string(T1, T2, T), !.
pos2(L, [H2|T2], [H2|T3], A) :- pos2(L, T2, T3, A).
% --------------------------------------------------------------

% General Code for lists: ---------------------------------------------------

empty_list([]) :- !, fail.
empty_list(_).

change_names([]).
change_names([H=H1|T]) :-
	H=H1,
	change_names(T).

% string-->term and term-->string.
string_to_term(S,T) :-
        mktemp('t2sXXXXXX',TMP),
        open_output(TMP,Out),
        write_string(S), 
%	write('.'),
        close_output(Out),
        open_input(TMP, In),
        read_term(T, [variable_names(V)]),
        close_input(In), 
        delete_file(TMP),
	change_names(V).

term_to_string(T,S) :-
        mktemp('t2sXXXXXX', TMP), 
        open_output(TMP,Out),
        write(T),
        close_output(Out),
        open_input(TMP, In),
        get_line(S),
        close_input(In), 
        delete_file(TMP).
%----------------------------------------

disp_list([]).
disp_list([H|T]) :- var(H), !,display(H), disp_list(T).
disp_list([nl|T]) :- !, nl, disp_list(T).
disp_list([H|T]) :- string(H), !, write_string(H), disp_list(T).
disp_list([H|T]) :- display(H), disp_list(T).
% ---------------------------------------------------------------

% File names handling:
:- export(file2Module/2).
:- export(absFile2Module/2).
:- export(fileName2ModuleName/2).
:- export(combineFileNameExt/2).
:- export(splitFileExt/3).
:- export(addFileExt/3).

% combineFileNameExt('asp.lp',asplp).
% combineFileNameExt(asp,asppl).
combineFileNameExt(FN,FE) :-
	file_name_extension(FN,F,E),
	atom_concat('.',E1,E),
	atom_concat(F,E1,FE).
combineFileNameExt(FN,FE) :-
	atom_concat(FN,'pl',FE).

%splitFileExt('asp.lp', asp, '.lp').
%splitFileExt(asp, asp,'').
splitFileExt(FileNameExt, FileName, E) :-
	file_name_extension(FileNameExt, FileName, E1),
	atom_concat('.',E,E1),
	!.
splitFileExt(FileName, FileName, '').

%addFileExt('asp.lp',_,'asp.lp').
%addFileExt(asp,'.aaaaa','asp.aaaaa').
addFileExt(FileNameExt, _, FileNameExt) :-
	file_name_extension(FileNameExt, _, _),!.
addFileExt(FileName, Ext, FileNameExt) :-
	atom_concat(FileName, Ext, FileNameExt).

% file2Module('asp.lp', 'asp.lp').
% file2Module('asp.pl','asp').
file2Module(FileName, Module) :-
	file_name_extension(FileName, Module, '.pl'),!.
file2Module(Module,Module).

% absFile2Module('/cygdrive/c/smodels/asp.pl', asp).
% absFile2Module('/cygdrive/c/smodels/asp.lp','asp.lp').
absFile2Module(F,M) :-
	no_path_file_name(F,F1),
	file2Module(F1,M).

% fileName2ModuleName('c:\\smodels\\sm12\\asp.lp','asp.lp').
% fileName2ModuleName('c:\\smodels\\sm12\\asp.pl',asp).
fileName2ModuleName(F,M) :-
	absolute_file_name(F,F1),
	no_path_file_name(F1,F2),
	file2Module(F2,M).

% e.g. aspName2IntName('/cygdrive/smodels/sm12/asp.lp','asplp.asp').
% or   aspName2IntName('asp.lp','asplp.asp').
:- export(aspName2IntName/2).
aspName2IntName(A,I) :-
	no_path_file_name(A,A1),
	splitFileExt(A1,A2,E),
	atoms_concat([A2,E,'.asp'],I).

:- export(aspName2AbsIntName/2).
aspName2AbsIntName(AspName, AbsInt) :-
	absFileName(AspName, AbsIntNameExt),
	splitFileExt(AbsIntNameExt, AbsIntName, Ext),
	atom_concat(AbsIntName, Ext, AbsIntNameE),
	atom_concat(AbsIntNameE, '.asp', AbsInt).

% file2Path('c:\\smodels\\sm12\\asp.lp','asp.lp','c:\\smodels\\sm12').
% file2Path('/cygdrive/c/sm12/asp.lp', 'asp.lp', '/cygdrive/ c/smodels/sm12/')
% file2Path('asp.pl', 'asp.pl', '/cygdrive/c/smodels/sm12/').
:- export(file2Path/3).
file2Path(A,FN,P) :-
	absolute_file_name(A,F),
	no_path_file_name(F,FN),
	atom_concat(P,FN,F).

:- export(absFileName/2).
:- export(winFileName/2).
absFileName(M, AbsFileNameExt) :-
	absolute_file_name(M, AbsFileName),
	addFileExt(AbsFileName, '.pl', AbsFileNameExt).

winFileName(M, WinFileNameExt) :-
	absFileName(M, AbsFileName),
	addFileExt(AbsFileName, '.pl', AbsFileNameExt),
	name(AbsFileNameExt, StringAbsFileNameExt),
	cyg2win(StringAbsFileNameExt, StringWinFileNameExt, swap),
	name(WinFileNameExt, StringWinFileNameExt),
% -------------------------------------------

writeList2File(_,[],_).
writeList2File(S,[H|T],C) :-
	put_comma(S,C),
	write(S,H),
	writeList2File(S,T,1).
% --------------------------------------------------------------------