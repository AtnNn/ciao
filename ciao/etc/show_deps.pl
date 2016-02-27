:- module( _ , _ , [assertions] ).

:- use_module(library(autoconfig)).

:- doc(title,"Showing file dependencies.").

:- doc( author, "David Trallero Mena" ).

:- doc( module, "This simple program takes a single Ciao or Prolog
   source filename (which is typically the main file of an
   application). It prints out the list of all the @concept{dependent
   files}, i.e., all files needed in order to build the application,
   including those which reside in libraries.

   A primary part prints out a list of files and its dependencies. The
   second part prints a tree will all dependencies. The leafs of the
   tree are nor expanded whenever they have been expanded in a
   previous branch." ).


:- use_module(library(lists)).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [working_directory/2]).
:- use_module(library(filenames), [
	                                no_path_file_name/2,
					basename/2
				      ] ).

:- use_module(library(ctrlcclean), 
	[ctrlc_clean/1,delete_on_ctrlc/2,ctrlcclean/0]).

:- use_module(library(compiler(c_itf)), 
	[
	 imports_pred/7,
	 process_file/7,
	 false/1,
	 cleanup_c_itf_data/0
	 ]).

:- use_module(library(errhandle), [error_protect/1]).  

% => res_1: this is for -l option
:- use_module(library(compiler), [use_module/1]).


:- data ftree/2.
:- data processed_file/1.
:- data related_file/1.

processed_file( user ).

:- data remove_files/1.

remove_files( [] ).


main( [] ) :-
	display_string("Usage: show_deps [options] module_name\n"
	||"Shows the dependency tree of the specified module.\n\n"
	||"   Options:\n"
	||"   -n: do not show default packages\n"
	||"   -l pathsfile: load path aliases from pathsfile\n" ).

main( X ) :-
	parse_args( X ).


parse_args( ['-n'|X] ) :-
	absolute_file_name( engine( arithmetic ),  AFile),
	process_file_top( AFile, ['-n'] ),
	findall( F, processed_file( F ), DefFiles ),
	retractall_fact( remove_files( _ ) ),
	asserta_fact( remove_files( DefFiles ) ),
	display( 'Found as default modules: ' ),
	display( DefFiles ),nl,nl,
	parse_args( X ).
parse_args( ['-l',File|X] ) :-
	absolute_file_name( File, AbsFile ),
	display( '{Loading path module: ' ),
	display( AbsFile ),
	display( '}\n' ),
	% +res_1
	use_module( AbsFile ),
	parse_args( X ).
parse_args( [X] ) :-
	process_file_top( X, [] ),
	make_tree( [X], [] , [X,Tree] ),
	to_module( Tree , TreeM ),
	display_tree( [X,TreeM] , [] ).



process_file_top( X, Opts ) :-
	cleanup_c_itf_data,
	retractall_fact( processed_file( _ ) ),
	asserta_fact( processed_file( user ) ),
	process_file( X, Opts ).


process_file( X, Opts ) :-
	\+ processed_file( X ),
	process_main_file( X ),
	findall( R , 
	         retract_fact( related_file( R ) ),
		 XFiles ),
	remove_files( RFiles ),
	difference( XFiles, RFiles, FFiles ),
        asserta_fact( ftree( X , FFiles ) ),
	to_module( FFiles , XModules ),
	( member( '-n' , Opts )
	->  true
	;   display( X ) , display( ' loads ' ), display( XModules ),nl,nl),
	process_related_files( FFiles, Opts ).
process_file( _, _ ).


process_related_files( [], _ ).
process_related_files( [X|Xs], Opts ) :-
	process_file( X, Opts ),
	process_related_files( Xs, Opts ).


% make_tree( [F|Fs] , Exclude , [ F , FFSons | Rest ] ) :-
% 	\+ member( F , Exclude ),
% 	!,
% 	current_fact( ftree( F , FSons ) ),
% 	difference( FSons , [F|Exclude] , FSons2 ),
% 	make_tree( FSons2 , [F|Exclude] , FFSons ),
% 	make_tree( Fs , Exclude , Rest ).
% make_tree( [_|Fs] , Exclude , O ) :-
% 	make_tree( Fs , Exclude , O ).
% make_tree( [] , _Exclude , [] ).


make_tree( [F|Fs] , Exclude , [ F | FRest ] ) :-
	atom( F ),
	current_fact( ftree( F , FSons ) ),
	!,
	(
            member( F , Exclude )
	->
	    ( FSons = [] -> FRest = Rest ; FRest = [ FSons | Rest ] ),
	    Exclude = NExclude
	;
	    append( [F|Fs] , Exclude , AllExclude ),
	    difference( FSons , AllExclude , FFSons ),
%	    to_module( AllExclude , AllEM ), display( exc( AllEM ) ),nl,
%	    to_module( FFSons , FFSonsM ), display( rec( FFSonsM ) ),nl,
	    make_tree( FFSons , AllExclude , FFSons2 ),
	    plain_list( FFSons2 , ESons ),
	    difference( ESons , Fs , CleanESons ),
	    union( CleanESons , [F|Exclude] , NExclude ),
	    difference( FSons , FFSons2 , FF ),
	    append( FF , FFSons2 , FFSons3 ),
	    ( FFSons3 = [] -> 
	      FRest = Rest 
	    ; 
	      FRest = [ FFSons3 | Rest] 
	    )
	),
	make_tree( Fs , NExclude , Rest ).
make_tree( [F|Fs] , Exclude , [F|O] ) :-
	make_tree( Fs , Exclude , O ).
make_tree( [] , _Exclude , [] ).




process_main_file( File ) :-
        error_protect(ctrlc_clean(
		process_file(File, asr, any, 
		             process_main_info_file, 
			     false, false, do_nothing )
				 )).

process_main_info_file( Base ) :- 
	assertz_fact( processed_file( Base ) ),
%	c_itf:comp_defines(Base),
	assert_related_files( Base ).

assert_related_files( Base ) :-
	imports_pred(Base,IM,_F,_A,_DefType,_Meta,_EndFile),
 	file_path(Base,CWD),
	working_directory(OldCWD,CWD),
 	absolute_file_name( IM ,'_opt' , '.pl' , CWD , _ , IMAbs , _ ),
	working_directory(_,OldCWD),
	\+ current_fact( related_file( IMAbs ) ),
	( \+ no_path_file_name( IMAbs , user ) -> true ),
	asserta_fact(related_file(IMAbs)),
	fail ; true.



file_path(Base,Path):-
	atom_codes(Base,Bases),
	no_path_file_name(Bases,Names),
	append(Paths,Names,Bases),
	atom_codes(Path,Paths).


do_nothing( _ ).


to_module( [] , [] ).
to_module( [F|Fs] , [Ft|Ms] ) :-
	list(F),
	!,
	to_module( F , Ft ),
	to_module( Fs , Ms ).
to_module( [F|Fs] , [M|Ms] ) :-
	no_path_file_name( F , M ),
	to_module( Fs , Ms ).



plain_list( [] , [] ).

plain_list( [L|Ls] , A ) :-
	L = [_|_],
	!,
	plain_list( L  , L1 ),
	plain_list( Ls, Ls1 ),
	union( Ls1 , L1 , A ).

plain_list( [L|Ls] , A ) :-
	plain_list( Ls , Ls1 ),
	union( [L] , Ls1 , A ).

	

%%%%%%%%%%% DISPLAY TREE





display_tree( [] , _ ).
display_tree( [ A | As ] , Depth ) :-
	(
	    list( A )
	->
	    ( As \= [] -> ND = [true] ; ND = [false] ),
	    append( Depth , ND , NDepth ),
            display_tree( A , NDepth )
	;
            display_tree_depth( Depth ),
            ( (As = [] ; As = [L], list(L)) ->
	      display_tree_last_element( A )
	    ;
	      display_tree_element( A )
	    )
	),
	display_tree( As, Depth ).

display_tree_element( A ) :-
	display( '|-- ' ),
	display( A ),
	nl.

display_tree_last_element( A ) :-
	display( '\\-- ' ),
	display( A ),
	nl.


display_tree_depth( false ) :-
	display( '    ' ),
	!.
display_tree_depth( true ) :-
        display( '|   ' ),
	!.
display_tree_depth( [] ) :-
	!.
display_tree_depth( [A|As] ) :-
	display_tree_depth( A  ),
	display_tree_depth( As ).

