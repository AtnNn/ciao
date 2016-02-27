:-module( mattr_local_code , [
	                      get_iattr/4,
	                      get_attr/3,
			      set_iattr/4,
			      set_attr/3,
			      detach_attr/3,
			      detach_iattr/3,
			      transform_attr/3
			     ] , [] ).


:- use_module( library( odd ) ).

:- use_module( library( 'mattr_global/mattr_global_code' ) ).


%%  predicates used by this module (Added by mattr_local_trans)
%%  $attr_hash$( pred , int ) => gives position of the vector (only if can_be_dynamic is false)

:- multifile '$max_num_attr$'/2.
:- multifile '$attr_hash$'/3.



%%  vectorized VERSION

between(N, Min, Max) :- integer(N), !, N >= Min, N =<  Max.
between(V, Min, Max) :- var(V), Min =< Max, between_nd(V, Min, Max).

between_nd(Min, Min, _).
between_nd(N, Min, Max) :-
        Min < Max, NMin is Min+1,
        between_nd(N, NMin, Max).


%%%%%%%%%%%%%%%
%% get_iattr %%
%%%%%%%%%%%%%%%


% Version with I ground
get_iattr( X , A , I , Name ) :-
	num( I ),
	!,
	bd_get_attr( X , Attr , Name ),
	arg( I , Attr , PosibleAt ),
	nonvar( PosibleAt ),
	copy_term( PosibleAt, OutAt ),
	OutAt = A.

% Version with I no ground => selecting everything
get_iattr( X , A , I , Name ) :-
	bd_get_attr( X , Attr , Name ),
	functor( Attr , _ , Arity ),
	between( I , 1 , Arity ),
	arg( I , Attr , A ),
	nonvar(A).


%%%%%%%%%%%%%%
%% get_attr %%
%%%%%%%%%%%%%%



% We could not solve it in compilation time, 
% let's do it in runtime
get_attr( X , A , Name ) :-
	nonvar(A),
	!,
	functor( A , AtF , AtAri ),
	'$attr_hash$'( Name , f(AtF,CheckAri) , N ),
	(
	    CheckAri == no 
	->
	    true
	;
	    (
		AtAri == CheckAri
	    -> 
	        true
	    ;
		message( error , 
		         ['Call to get_attr(',X,',',A,
			  '), does not match with attribute declaration:', 
			  AtF , '/' , CheckAri] )
	    )
	),
	get_iattr( X , A , N , Name ).


% A is not specified => get all attributes by fail
get_attr( X , A , Name ) :-
	'$attr_hash$'( Name , _ , N ),
	get_iattr( X , A , N , Name ),
	nonvar(A).




%%%%%%%%%%%%%%%
%% set_iattr %%
%%%%%%%%%%%%%%%


set_iattr( X , A , I , Name ) :-
	num( I ),
	!,
	(
	    bd_get_attr( X , Attr , Name )
	->
	    % compute number of attr set
	    arg( I , Attr , AB ), % AB=Attr before
	    (
		var(AB)
	    ->
	        arg(1 , Attr , N ),
		N1 is N + 1,
		setarg( 1 , Attr , N1 )
	    ;
		true
	    ),
	    setarg( I , Attr , A),
	    bd_set_attr( X , Attr , Name )	    
	;
	    % we still have no attribute in global attribute
	    '$max_num_attr$'( Name , NMax ),
	    functor( V , Name , NMax ),
	    setarg( I , V , A ),
	    setarg( 1 , V , 1 ),
	    bd_set_attr( X , V , Name )
	).


%%%%%%%%%%%%%%
%% set_attr %%
%%%%%%%%%%%%%%


set_attr( X , A , Name ) :-
	functor( A , AtF , AtAri ),
	'$attr_hash$'( Name , f(AtF,CheckAri) , N ),
	(
	    CheckAri == no 
	->
	    true
	;
	    (
		AtAri == CheckAri
	    -> 
	        true
	    ;
		message( error , 
		         ['Call to set_attr(',X,',',A,
			  '), does not match with attribute declaration:', 
			  AtF , '/' , CheckAri] ),
		fail
	    )
	),
 	set_iattr( X , A , N , Name ).




%%%%%%%%%%%%%%%%%
%% detach_attr %%
%%%%%%%%%%%%%%%%%


detach_attr( X , A , Name ) :-
       functor( A , AtF , AtAri ),
       !,
       '$attr_hash$'( Name , f(AtF,CheckAri) , N ),
       (
	    CheckAri == no 
       ->
	    true
       ;
	    (
		AtAri == CheckAri
	    -> 
	        true
	    ;
		message( error , 
		         ['Call to detach_attr(',X,',',A,
			  '), does not match with attribute declaration:', 
			  AtF , '/' , CheckAri] ),
		fail
	    )
       ),
       detach_iattr( X , N , Name ).

	
%%%%%%%%%%%%%%%%%%
%% detach_iattr %%
%%%%%%%%%%%%%%%%%%


detach_iattr( X , I , Name ) :-
	num( I ),
	bd_get_attr( X , Attr , Name ),
	% get number of attr set
        arg( 1 , Attr , N ),
	N1 is N - 1,
	(
	    N1 == 0
	->
	    mattr_global_code:detach_attr( X , Name )
	;
	    setarg( I , Attr , _  ),
	    setarg( 1 , Attr , N1 ),
	    bd_set_attr( X , Attr , Name )
	).


%%%%%%%%%%%%%%%
%% utilities %%
%%%%%%%%%%%%%%%


remove_if_unify( _ , [] , [] ) :- !.
remove_if_unify( A , [A0|R] , S ) :- 
	copy_term(A , A1 ),
	copy_term(A0, A1 ),!,
	remove_if_unify( A , R , S ).

remove_if_unify( A , [B|R] , [B|S] ) :- !,
	remove_if_unify( A , R , S ).

	



:- use_module( library( odd ) ).

transform_attr( X , Y , 0 ) :-
	!,
	functor( X , N , A ),
	functor( Y , N , A ),
	transform_attr( X , Y , 1 ).

transform_attr( X , Y , N ) :-
	arg( N , X , XAN ),
	!,
	(
	    var(XAN)
	->
	    arg( N , Y , no )
	;
	    arg( N , Y , XAN )
	),
	N1 is N + 1,
	transform_attr( X , Y , N1 ).

transform_attr( _ , _ , _ ).
