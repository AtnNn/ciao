:- module( mattr_global_trans , 
	 _,
	 [assertions,dcg] ).

:- use_module( library(lists) ).

:- data attr_priority/1.

attr_priority( '500' ).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRANSFORMING CLAUSES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%


%% FOR :- attributes
%% --- :- attribute at1,at/2 : priority(200) => imprimir el numero con TRES digitos
mattr_def( (:- attribute_priority(P))  ,
	   CODE,
	    M ) :-
	(
	    num( P )
	->
	    check_limits( P , NewPrio ),
            retractall_fact( attr_priority(  _   ) ),
	    asserta_fact(    attr_priority( NewPrio ) ),
	    generate_prio_module( M , Key ),
	    CODE = (:- '$attribute_local'(Key))
	;
	    CODE = _,
	    message( error , 
	    ['attribute_priority: Syntas is: ',
	     '`:- attribute_priority <positive number < 1000>'])
	).


%% FOR adding code at the end of the file :D
mattr_def( end_of_file, [
	                (:-( multifile('$combine_attr'/4) )),
			(:-( multifile('$check_attr'/3) )),
			('$combine_attr'( Key , MA1 , MA2 , O ) :-
		           M:combine_attributes( MA1 , MA2 , O)),
			('$check_attr'( Key , MA1 , X ) :-
			   M:check_attr( MA1 , X )),
			   end_of_file
		       ],
		       M ) :-
	generate_prio_module( M , Key ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Precalculate attributes we can
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mattr_redef( get_attr( X ,A ), mattr_global_code:get_attr( X , A , PM ) , M ) :-
	generate_prio_module( M , PM ).

mattr_redef( set_attr( X , A ), mattr_global_code:set_attr( X , A , PM ) , M) :-
	generate_prio_module( M , PM ).

mattr_redef( detach_attr( X ), mattr_global_code:detach_attr( X , PM ) , M) :-
	generate_prio_module( M , PM ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END of code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_3digitn( AN, AN3 ) :-
	atom_length( AN , L ),
	concat_ceroes( L , AN , AN3 ).

concat_ceroes( 3 , AN , AN  ).
concat_ceroes( 2 , AN , AN3 ) :- atom_concat( '0' , AN , AN3 ).
concat_ceroes( 1 , AN , AN3 ) :- atom_concat( '00' , AN , AN3 ).


generate_prio_module( M , PM ) :-
	attr_priority( P ) ,
	generate_3digitn( P , P3D ),
	atom_concat( P3D , M , PM ).


check_limits( P ,   0 ) :- 
	P < 0 ,
	!,
	message( error,
	         ['Priority should be between 0 and 999, rounding it to 0'] ).

check_limits( P , 999 ) :- 
	P > 999 , 
	!,
	message( error,
	         ['Priority should be between 0 and 999, rounding it to 999'] ).

check_limits( P , P ).








