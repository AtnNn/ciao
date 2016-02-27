:- module( mattr_local_trans , 
	%[get_position/4,mattr_def/3,mattr_redef/3] , 
	 _,
	 [assertions,dcg] ).

:- use_module( library(lists) ).

:- data attr_convert/4.

:- data attr_priority/1.

attr_priority( '500' ).


%attr_convert( [] , 0 , false , '$$$no_module'  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRANSFORMING CLAUSES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%


%% FOR :- attributes
%% --- :- attribute at1,at/2 : priority(200) => imprimir el numero con TRES digitos
mattr_def( (:- attribute(R)) , _ , M ) :-
	transform_into_list( R , L , RL , [] ),
        retractall_fact( attr_convert(  _ , _ ,   _  , M) ),
        asserta_fact(    attr_convert( RL , L , true , M) ).


%% FOR adding code at the end of the file :D
mattr_def(end_of_file, [
	                :-( multifile('$max_num_attr$'/2) ),
			:-( multifile('$attr_hash$'/3) ),
                        :-( multifile(combine_attributes/3) ),
                        :-( multifile(verify_attribute/2) ),
			'$max_num_attr$'( M , L )
		       | Hash
		       ],
		       M ) :-
	generate_hash_table( M , Hash , L ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Precalculate attributes we can
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mattr_redef( mattr_global_code:get_attr(  X , A , PM     ) ,
	     mattr_local_code: get_iattr( X , A , I , PM ) , _ ) :-
	nonvar( A ),
	!,
	get_index( A , I , _M ).

mattr_redef( mattr_global_code:get_attr(  X , A , PM ) ,
	     mattr_local_code: get_attr(  X , A , PM ) , _M ).




mattr_redef( mattr_global_code:set_attr( X , A ,     PM ),
	     mattr_local_code:set_iattr( X , A , I , PM ) , M ) :-
	nonvar(A),
	!,
	get_index( A , I , M ).

mattr_redef( mattr_global_code:set_attr( X , A , PM ),
	     mattr_local_code: set_attr( X , A , PM) , _M ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END of code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_hash_table( M , Hashlist , Len ) :-
	attr_convert( AttrList , Len , _ , M ),
	g_h_t( M , AttrList , 1 , Hashlist ),
	retract_fact( attr_convert(_ , Len , _ , M) ).


generate_hash_table( _ , [] , 0 ) :-
	message( note, 
['package attribute included and no \':- attribute\' declaration has been found']).


% Generatet Hast Table
g_h_t( _ , [] , _ , [end_of_file] ).

g_h_t( Module , [Pred/Ari |R] , N , 
       ['$attr_hash$'( Module , Pred/Ari , N ) | R2] 
     ) :-
        !,
	N1 is N + 1,
	g_h_t( Module , R , N1 , R2 ).

g_h_t( Module , [Pred|R] , N , [ '$attr_hash$'( Module , Pred/no , N ) | R2 ] ) :-
	N1 is N + 1,
	g_h_t( Module , R , N1 , R2 ).




transform_into_list( (A,B) , L ) --> 
	transform_into_list( A , L1 ),
	transform_into_list( B , L2 ),
	{L is L1 + L2}.

transform_into_list( A/N , 1 ) --> [ A/N ].

transform_into_list( A   , 1 ) --> [ A ].



% imprimir_global :-
% 	attr_convert( A , B , C , D ),
% 	mattr_local_trans:user_inform( [attr_convert( A,B,C,D )] ),
% 	fail.
% imprimir_global :- mattr_local_trans:user_inform( [fin] ).

get_index( At , I , M ) :-
	attr_convert( L , _ , true , M ),
	functor( At , Attr , Ari ),
	get_position( Attr/Ari , L , I , M  ).

% for avoid messages => no debugging
% user_inform( _ ) :- !.

%user_inform( [] ) :- display( '\n' ).
%user_inform( [A|R] ) :-
%	display( A ) , 
%	user_inform( R ).

get_position( At/_ , [At|_] , 1 , _ ) :- !.

get_position( At/X , [ At/A |_] , 1 , M ) :- 
%D	message( note , ['paso1', At/X , At/A ] ),
	!,
	(  X==A -> true ; message( error , ['Atribute \'' , At , '/' ,X , 
	                               '\' defined in module \'' , M ,
				       '\' with different arity, check it out: \'',
				       At,'/',A , '\''] ) ).

get_position( A/N , [] , 1 , M ) :- 
	!,
        message( error , [ 'Atribute \'' , A , '/' , N ,
	                   '\' not defined in module \'' , M , '\''
			 ] ).

get_position( At/Ari , [_|R] , N1 , M ) :- 
	!,
%D	message( note , ['paso2', At/Ari , R] ),
	get_position( At/Ari , R , N , M ),
	N1 is N + 1.




