 :- module(fillcontent_ciao, [main/1], []).

:- use_module(library(write)).
:- use_module(library(strings), [write_string/1]).
:- use_module(library(format)).
:- use_module(library(system)).
:- use_module(library(file_utils)).
:- use_module(library(pillow(html))).
:- use_module(library(dynamic)).
:- use_module(library(lists), [append/3]).

:- dynamic constant/2.
:- dynamic print_value/1.

constant(url,'http://ciao-lang.org/').
constant(foot,'For more information please contact <a href=\"webmaster\@clip.dia.fi.upm.es\">webmaster\@clip.dia.fi.upm.es</a>').

anchor_string("<!MARK!MARK!MARK!>"). % The string that we will find to cut the head or footer.

main([CiaoFile, MailmanFile, PrintValue]) :-
	% TODO: use a data predicate and assert mailman file, print value
	%       (that would make tr/2 simpler)
	assertz(print_value(PrintValue)),
	%
	file_to_string(CiaoFile, S),
        read_files(MailmanFile),
	xml2terms(S, T0),
        set_prolog_flag(write_strings,on),
	tr(T0, T),
	output_to_string(output_html(T), String0),
	cut_output(PrintValue, String0, String),
	%
	write_string(String).
%	atom_concat(MailmanFile, '.html', OutFile),
%	string_to_file(String, OutFile).

insert_mark(all, String, String).
insert_mark(no_footer, String0, String) :-
	anchor_string(Mark),
	append(String0, Mark, String).
insert_mark(no_head, String0, String) :-
	anchor_string(Mark),
	append(Mark, String0, String).

cut_output(all, String, String).
cut_output(no_footer, String0, String) :-
	anchor_string(Mark),
	append(Mark, _, Mark2),
	append(String, Mark2, String0).
cut_output(no_head, String0, String) :-
	anchor_string(Mark),
	append(Mark, String, Mark2),
	append(_, Mark2, String0).

% Note: this predicate mixes lists and environments (it can be
% improved)

%:tr(R,_) :- display(r(R)), nl, fail.
tr(R, R) :- var(R), !. % this case should not happen
tr(R, R) :- R = declare(_), !.
tr(R, R) :- R = '$'(_,_), !.
tr(R, R) :- is_string_fast(R), !.
tr([E0|Es0], [E|Es]) :- tr(E0, E), tr(Es0, Es).
% [debug code]
% tr(A, _) :-
% 	push_prolog_flag(write_strings, on),
% 	display('>>> '), write(A), nl,
% 	fail.
tr(env(title, Attr, _), R) :- !,
        constant(title,Content),
	R = env(title, Attr, Content).
tr(env(a, Attr, Content0), R) :- !,     
        ut_member(href=HRef,Attr),
        add_url(HRef,NewRef),
        ut_replace(href,NewRef,Attr,Attr2),
	R = env(a, Attr2, Content),
	tr(Content0, Content).
tr(elem(img, Attr), R) :-      
        ut_member(src=HRef,Attr),!,
        add_url(HRef,NewRef),
        ut_replace(src,NewRef,Attr,Attr2),
 	R = elem(img, Attr2).
tr(elem(link, Attr), R) :- 
        ut_member(href=HRef,Attr),!,
        add_url(HRef,NewRef),
        ut_replace(href,NewRef,Attr,Attr2),
	R = elem(link, Attr2).
tr(env(div, Attr, _), R) :- 
        atom_codes('main',Codes),
        ut_member(class=Codes,Attr),!,
        constant(content,Content0),
	print_value(PrintValue),
	insert_mark(PrintValue, Content0, Content),
 	R = env(div, Attr, Content).
tr(env(div, Attr, _), R) :-
        atom_codes('footer',Codes),
        ut_member(class=Codes,Attr),!,
        constant(foot,Content),
        R = env(div, Attr, Content).
tr(env(Tag, Attr, Content0), R) :- !,
	% Default case (apply transformation transitively)
	R = env(Tag, Attr, Content),
	tr(Content0, Content).
tr(elem(Tag, Attr), R) :- !,
	% Default case (apply transformation transitively)
	R = elem(Tag, Attr).
tr(Env, _) :-
	% Case for unrecognized terms
	display('Wrong env: '),
	display(Env),
	nl,
	fail.

% fast check for string (just checks that the first element is a code)
is_string_fast([]).
is_string_fast([X|_]) :- number(X).

read_files(File):-
    ut_atoms_concat([File,'.content'],ContentFile),
    ut_atoms_concat([File,'.title'],TitleFile),
    file_to_string(ContentFile,Content),
    file_to_string(TitleFile,Title),
    assertz(constant(content,Content)),
    assertz(constant(title,Title)).


add_url(S,S0):-
    constant(url,URL),
    atom_codes(URL,URLCodes),
    append(URLCodes,S,S0).

ut_member(X,[X|_]).
ut_member(X,[_|Xs]) :- ut_member(X,Xs).

ut_replace(_, _, [], []).
ut_replace(Tag, Info, [Tag=_|T], [Tag=Info|T2]) :- !,ut_replace(Tag, Info, T, T2).
ut_replace(Tag, Info, [H|T], [H|T2]) :- ut_replace(Tag, Info, T, T2).

ut_atoms_concat([],'').
ut_atoms_concat([A|As],Atom) :-
        ut_atoms_concat(As,Atom_aux),
        ut_atom_num_concat(A,Atom_aux,Atom).

ut_atom_num_concat(A,B,C) :-
	( atom(A) -> atom_codes(A,As); number_codes(A,As) ),
	( atom(B) -> atom_codes(B,Bs); number_codes(B,Bs) ),
	append(As,Bs,Cs),
	atom_codes(C,Cs).
	  

ut_atom_concat(A,B,C) :-
	ut_atom_concat_aux(A,As),
	ut_atom_concat_aux(B,Bs),
	ut_atom_concat_aux(C,Cs),
	append(As,Bs,Cs),
	atom_codes(A,As),
	atom_codes(B,Bs),
	atom_codes(C,Cs).

ut_atom_concat_aux(A,As)   :- atom(A), !, atom_codes(A,As).
ut_atom_concat_aux(_A,_As).

% ---------------------------------------------------------------------------

:- meta_predicate output_to_string(goal, String).
% Capture the output of goal G in string String
output_to_string(G, String) :-
	TempFile = 'out.tmp',
	output_to_file(G, TempFile),
	file_to_string(TempFile, String),
	delete_file(TempFile).

