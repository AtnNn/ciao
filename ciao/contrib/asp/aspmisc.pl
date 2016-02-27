:- module(aspmisc, []).

:- export(empty_list/1).
:- export(string_to_term/2).
:- export(term_to_string/2).
:- export(pos/4).
:- export(pos2/4).
:- export(disp_list/1).
:- export(atoms_concat/2).
:- export(number2atom/2).
:- export(chk_file_exist/1).
:- export(must_exist/1).

:- use_module(library(strings)).
:- use_module(library(streams)).

% General Code for lists: ---------------------------------------------------

empty_list([]) :- !, fail.
empty_list(_).


% string-->term and term-->string.
string_to_term(S,T) :-
        mktemp('t2sXXXXXX',TMP),
        open_output(TMP,Out),
        write_string(S), 
%	write('.'),
        close_output(Out),
        open_input(TMP, In),
        read(T),
        close_input(In), 
        delete_file(TMP).

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

disp_list([]).
disp_list([H|T]) :- var(H), !,display(H), disp_list(T).
disp_list([nl|T]) :- !, nl, disp_list(T).
disp_list([H|T]) :- display(H), disp_list(T).
% ---------------------------------------------------------------

% concat a list of atoms all together
atoms_concat([], '') :- !.
atoms_concat([[]|T], C) :- 
       atoms_concat(T,C),!.
atoms_concat([H|T], C) :-
	atoms_concat(T, C1),
	atom_concat(H, C1, C).
% -----------------------------------------------------------------

% -------------------------------------------------
% convert number into atom.
% ------------------------------------------------
number2atom(Number,Atm) :-
    number_chars(Number, Char_List),
    atoms_concat(Char_List, Atm), !.
% -------------------------------------

% --------------------------------------------
% file existence check.
% -------------------------------------------
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
