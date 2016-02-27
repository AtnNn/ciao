/*  $Id: chr_swi_bootstrap.pl,v 1.8 2006/03/03 06:42:41 bmd Exp $

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(_,
	  [ chr_compile_step1/2		% +CHRFile, -PlFile
	  , chr_compile_step2/2		% +CHRFile, -PlFile
	  , chr_compile_step3/2		% +CHRFile, -PlFile
	  , chr_compile_step4/2		% +CHRFile, -PlFile
	  , chr_compile/4
	  , do_bootstrap/0
	  , do_bootstrap1/0
	  , do_bootstrap2/0
	  , do_bootstrap3/0
	  , do_bootstrap4/0
	  , do_bootstrap42/0
	  , do_bootstrap43/0
	  ]).


%% Ciao begin
:- use_module(library(write)). % portray_clause/2

:- set_prolog_flag( multi_arity_warnings   , off ).
:- set_prolog_flag( discontiguous_warnings , off ).

%% Ciao end
:- include(chr_op).

		 /*******************************
		 *    FILE-TO-FILE COMPILER	*
		 *******************************/

%	chr_compile(+CHRFile, -PlFile)
%	
%	Compile a CHR specification into a Prolog file

chr_compile_step1(From, To) :-
	use_module('chr_translate_bootstrap.pl'),
	chr_compile(From, To, informational, chr_translate_bootstrap ).
chr_compile_step2(From, To) :-
	use_module('chr_translate_bootstrap1.pl'),
	chr_compile(From, To, informational, chr_translate_bootstrap1 ).
chr_compile_step3(From, To) :-
	use_module('chr_translate_bootstrap2.pl'),
	chr_compile(From, To, informational, chr_translate_bootstrap2 ).
chr_compile_step4(From, To) :-
	use_module('chr_translate.pl'),
	chr_compile(From, To, informational, chr_translate ).

chr_compile(From, To, _MsgLevel, M) :-
	read_chr_file_to_terms(From,Declarations),
	M:chr_translate(Declarations, Declarations1),
	insert_declarations(Declarations1, NewDeclarations),
	writefile(To, From, NewDeclarations).


%% SWI begin
% specific_declarations([:- use_module('chr_runtime'),
% 		       :- style_check(-singleton),
% 		       :- style_check(-discontiguous)|Tail], Tail).
%% SWI end

%% SICStus begin
%% specific_declarations([(:- use_module('chr_runtime')),
%%                     (:-use_module(chr_hashtable_store)),
%% 		       (:- use_module('hpattvars')),
%% 		       (:- use_module('b_globval')),
%% 		       (:- use_module('hprolog')),  % needed ?
%%                     (:- use_module(library(terms),[term_variables/2])),
%% 		       (:- set_prolog_flag(discontiguous_warnings,off)),
%% 		       (:- set_prolog_flag(single_var_warnings,off))|Tail], Tail).

specific_declarations([(:- include( library( 'chr/chr_op' ) )),
		       (:- use_module( library( 'chr/chr_runtime' ) )),
		       (:- use_module( library( 'chr/hprolog' ) )),
		       (:- use_package( library( swi_mattr_global ) )),
		       (:- use_module( library( odd      ) , [ setarg/3   ] )),
		       (:- use_module( library( sort     ) , [ sort/2     ] )),
		       (:- use_module( library( write    ) , [ print/1    ] )),
		       (:- use_module( library( iso_misc ) , [ compound/1 ] )),
		       (:- use_module( library( sets     ) , [ merge/3    ] )),
		       (:- use_module( library( 'chr/chr_hashtable_store'))),
		       (:- set_prolog_flag( multi_arity_warnings   , off )),
		       (:- set_prolog_flag( discontiguous_warnings , off ))
		       |Tail], Tail).
%% SICStus end



insert_declarations(Clauses0, Clauses) :-
	specific_declarations(Decls,Tail),
	(Clauses0 = [(:- module(M,E))|FileBody] ->
	    Clauses = [ (:- module(M,E))|Decls],
	    Tail = FileBody
	;
	    Clauses = Decls,
	    Tail = Clauses0
	).

%	writefile(+File, +From, +Desclarations)
%	
%	Write translated CHR declarations to a File.

writefile(File, From, Declarations) :-
	open(File, write, Out),
	writeheader(From, Out),
	writecontent(Declarations, Out),
	close(Out).

writecontent([], _).
writecontent([D|Ds], Out) :-
	portray_clause(Out, D),		% SWI-Prolog
	writecontent(Ds, Out).


writeheader(File, Out) :-
	format(Out, '/*  Generated by CHR bootstrap compiler~n', []),
	format(Out, '    From: ~w~n', [File]),
%	format_date(Out),
	format(Out, '    DO NOT EDIT.  EDIT THE CHR FILE INSTEAD~n', []),
	format(Out, '*/~n~n', []).

%% SWI begin
% format_date(Out) :-
% 	get_time(Now),
% 	convert_time(Now, Date),
% 	format(Out, '    Date: ~w~n~n', [Date]).
%% SWI end

%% SICStus begin
%% :- use_module(library(system)).
%% format_date(Out) :-
%% 	datime(datime(Year,Month,Day,Hour,Min,Sec)),
%% 	format(Out, '    Date: ~d-~d-~d ~d:~d:~d~n~n', [Day,Month,Year,Hour,Min,Sec]).
%% SICStus end



		 /*******************************
		 *	       MESSAGES		*
		 *******************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_chr_file_to_terms(Spec, Terms) :-
	chr_absolute_file_name(Spec, [ access(read) ], Path),
	open(Path, read, Fd, []),
	read_chr_stream_to_terms(Fd, Terms),
	close(Fd).

read_chr_stream_to_terms(Fd, Terms) :-
	chr_local_only_read_term(Fd, C0, [ module(chr) ]),
	read_chr_stream_to_terms(C0, Fd, Terms).

read_chr_stream_to_terms(end_of_file, _, []) :- !.
read_chr_stream_to_terms(C, Fd, [C|T]) :-
	( ground(C),
	  C = (:- op(Priority,Type,Name)) ->
		op(Priority,Type,Name)
	;
		true
	),
	chr_local_only_read_term(Fd, C2, [module(chr)]),
	read_chr_stream_to_terms(C2, Fd, T).




%% SWI begin
chr_local_only_read_term(A,B,_C) :- read_term(A,B,[]).
% chr_absolute_file_name(A,B,C) :- absolute_file_name(A,B,C).
%% Ciao begin
%:- use_module( library( filenames ) ).
chr_absolute_file_name(A,_B,C) :- absolute_file_name(A,C).
%% Ciao end
%% SWI end

%% SICStus begin
%% chr_local_only_read_term(A,B,_) :- read_term(A,B,[]).
%% chr_absolute_file_name(A,B,C) :- absolute_file_name(A,C,B).
%% SICStus end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(compiler), [use_module/1, unload/1]).
:- use_module(library(compiler(c_itf_internal)), 
	    [ cleanup_c_itf_data/0, cleanup_itf_cache/0 ] ).

do_bootstrap :-
	display( 'Doing bootstrap 1\n' ),
	do_bootstrap1,
	display( 'Doing bootstrap 2\n' ),
	do_bootstrap2,
	display( 'Doing bootstrap 3\n' ),
	do_bootstrap3,
	unload( 'chr_translate_bootstrap.pl' ),
	unload( 'chr_translate_bootstrap1.pl' ),
	unload( 'chr_translate_bootstrap2.pl' ),
	cleanup_itf_cache,
	display( 'Doing bootstrap 4\n' ),
	do_bootstrap4,
	unload( 'chr_translate_bootstrap2.pl' ),
	cleanup_itf_cache,
	do_bootstrap42,
	unload( 'chr_translate.pl' ),
	cleanup_itf_cache,
	do_bootstrap43.

do_bootstrap1 :-
	chr_compile_step1('chr_translate_bootstrap1.chr', 'chr_translate_bootstrap1.pl' ),
	chr_compile_step2('chr_translate_bootstrap1.chr', 'chr_translate_bootstrap1.pl' ).

do_bootstrap2 :-
        chr_compile_step2('chr_translate_bootstrap2.chr', 'chr_translate_bootstrap2.pl' ),
        chr_compile_step3('chr_translate_bootstrap2.chr', 'chr_translate_bootstrap2.pl' ).

do_bootstrap3 :-
        chr_compile_step3('guard_entailment.chr', 'guard_entailment.pl' ).

do_bootstrap4 :-
	display( 'Bootstrap 4.1\n' ),
        chr_compile_step3('chr_translate.chr', 'chr_translate.pl' ).

do_bootstrap42 :-
	display( 'Bootstrap 4.2\n' ),
        chr_compile_step4('guard_entailment.chr', 'guard_entailment.pl' ).

do_bootstrap43 :-
	display( 'Bootstrap 4.3\n' ),
        chr_compile_step4('chr_translate.chr', 'chr_translate.pl' ).
	
