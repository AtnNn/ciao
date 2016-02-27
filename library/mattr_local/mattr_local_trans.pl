:- module( mattr_local_trans , 
	[lmattr_def/3,lmattr_redef/3] , 
%	 _,
	 [assertions,dcg] ).

:- use_module( library( lists ) ).
:- use_module( library( odd   ) ). % setarg

:- data attr_convert/3.
:- data attr_priority/1.

attr_priority( '500' ).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRANSFORMING CLAUSES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%


lmattr_def( (:- attribute(R)) , _ , M ) :-
	transform_into_list( R , L , RL , [] ),
        retractall_fact( attr_convert(  _ , _ , M) ),
        asserta_fact(    attr_convert( RL , L , M) ).

lmattr_def( (:- '$attribute_local'(R)) , _ , _ ) :-
        retractall_fact( attr_priority(  _ ) ),
        asserta_fact(    attr_priority( R ) ).

%% FOR adding code at the end of the file :D
lmattr_def( end_of_file, 
	               [
	                :-( multifile('$max_num_attr$'/2) ),
			:-( multifile('$attr_hash$'/3) ),
			'$max_num_attr$'( PM , L )
		       | Hash
		       ],
		       M ) :-
	generate_prio_module( M , PM ),
	generate_hash_table( M , Hash , L ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Precalculate attributes we can
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lmattr_redef( mattr_global_code:get_attr(  X , A , PM     ) ,
	      mattr_local_code: get_iattr( X , A , I , PM ) , _ ) :-
	nonvar( A ),
	!,
	get_index( A , I , _M ).

lmattr_redef( mattr_global_code:get_attr(  X , A , PM ) ,
	      mattr_local_code: get_attr(  X , A , PM ) , _M ).




lmattr_redef( mattr_global_code:set_attr( X , A ,     PM ),
	      mattr_local_code:set_iattr( X , A , I , PM ) , M ) :-
	nonvar( A ),
	!,
	get_index( A , I , M ).

lmattr_redef( mattr_global_code:set_attr( X , A , PM ),
	      mattr_local_code: set_attr( X , A , PM) , _M ).




lmattr_redef( mattr_global_code:detach_attr( X , A , PM ),
	      mattr_local_code:detach_iattr( X , I , PM ) , M ) :-
	nonvar( A ),
	!,
	get_index( A , I , M ).

lmattr_redef( mattr_global_code:detach_attr( X , A , PM ),
	      mattr_local_code: detach_attr( X , A , PM) , _M ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END of code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_hash_table( M , NewCodeList , L1 ) :-
	attr_convert( AttrList , Len , M ),
	generate_prio_module( M , PM ),
	NewCodeList = [ (:-(function('@'/2))) | FunctionList ],
	functor( F , PM , Len ),
	g_f_t( AttrList , F  , 1 , FunctionList , HashList ),
	g_h_t( AttrList , PM , 2 , HashList ),
	retract_fact( attr_convert(_ , Len , M) ),
	L1 is Len + 1,
	!.


generate_hash_table( _ , [] , 0 ) :-
	message( note, 
['package mattr_local used and no \':- attribute\' declaration was found']).


% Generate Function Table. Example:
%
%  :- function @/2.
%
%  @(at1,test_local1(A, _, _)) := A.
%  @(at2,test_local1(_, B, _)) := B.
%  @(at3,test_local1(_, _, C)) := C.


g_f_t( [] , _ , _ , A , A ).

g_f_t( [P | R] ,  F , N , [ '@'( Attr , Func , V)  | R2] , E ) :-
	copy_term( F , Func ) , 
	setarg( N , Func , V ),
	(P = _/_ -> P = Attr/_ ; P = Attr),
	N1 is N + 1,
	g_f_t( R , F , N1 , R2 , E ).




% Generate Hash Table
g_h_t( [] , _ , _ , [end_of_file] ).

g_h_t( [P | R] , Module , N , ['$attr_hash$'( Module , P2 , N ) | R2] ) :-
	( P  = Pred/Ari ->  P2 = f(Pred,Ari)  ;  P2 = f(P,no)  ),
	N1 is N + 1,
	g_h_t( R , Module , N1 , R2 ).


% g_h_t( Module , [Pred/Ari | R] , N , 
%        ['$attr_hash$'( Module , Pred/Ari , N ) | R2] 
%      ) :-
%         !,
% 	N1 is N + 1,
% 	g_h_t( Module , R , N1 , R2 ).

% g_h_t( Module , [Pred|R] , N , [ '$attr_hash$'( Module , Pred/no , N ) | R2 ] ) :-
% 	N1 is N + 1,
% 	g_h_t( Module , R , N1 , R2 ).




generate_3digitn( AN, AN3 ) :-
	atom_length( AN , L ),
	concat_ceroes( L , AN , AN3 ).




concat_ceroes( 3 , AN , AN  ).
concat_ceroes( 2 , AN , AN3 ) :- atom_concat( '0'  , AN , AN3 ).
concat_ceroes( 1 , AN , AN3 ) :- atom_concat( '00' , AN , AN3 ).




generate_prio_module( M , PM ) :-
	attr_priority( P ) ,
	generate_3digitn( P , P3D ),
	atom_concat( P3D , M , PM ).




transform_into_list( (A,B) , L ) --> 
	transform_into_list( A , L1 ),
	transform_into_list( B , L2 ),
	{L is L1 + L2}.

transform_into_list( A/N , 1 ) --> [ A/N ].

transform_into_list( A   , 1 ) --> [ A ].




get_index( At , I1 , M ) :-
	attr_convert( L , _ , M ),
	functor( At , Attr , Ari ),
	get_position( Attr/Ari , L , I , M  ),
	I1 is I + 1.




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





%% *** Delete this comment after reading: it is only a reminder! ***
%% 
%% The "assertions" library needs to be included in order to support
%% ":- comment(...,...)." declarations such as below, i.e., insert: 
%% 
%% :- module(_,_,[assertions]).
%% 
%% At the beginning of the file:
%% The following version comment(s) can be moved elsewhere in the 
%% file. Subsequent version comments will always be placed above 
%% the last one inserted.


:- comment(version_maintenance,on).

