:- module(foreign_inliner_tr,[foreign_inliner_tr/3],[assertions]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(write),[write/2]).
:- use_module(library('compiler/c_itf'),[defines_module/2]).


:- data opened/0.
:- data inliner_dest/2.

foreign_inline_file(Module,InlineFile):-
	defines_module(Base,Module),
	atom_concat(Base,'_inline.c',InlineFile).

foreign_inliner_tr(end_of_file, end_of_file, M) :-
	(   opened ->
	    inliner_dest(S,M),
	    close(S),
	    retractall_fact(opened)
	;
	    true
	),
	retractall_fact(inliner_dest(_,M)).

foreign_inliner_tr((:- foreign_inline(Term,Text)), Clause, M) :-
	(   inliner_dest(S,M) ->
	    Clause = []
	;
	    % The next will obtain the Destination Stream for the inline functions
	    foreign_inline_file(M,InlineFile),
	    open(InlineFile,write,S),
	    write(S,'/\* Warning: This file has been automatically generated from '),
	    write(S,M),
	    write(S,'.pl \*/\n'),
	    assertz_fact(inliner_dest(S,M)),
	    atom_concat(M,'_inline',MI),
	    Clause = [(:- use_foreign_source([MI]))]
	),
	atom_codes(AText,Text),
	write(S,'/\*'),
	( var(Term) -> write(S, 'Global declaration') ; write(S,Term)),
	write(S,'\*/\n'),
	write(S,AText).


:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*11+215,2004/03/26,19:43*28+'CET'), "Added this
   file to let to write inline C code in a prolog module.  (Edison
   Mera)").

