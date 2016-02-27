
%:- use_module( library( term_basic ) , [ functor/3 ] ).

:- multifile 
        verify_attribute/2,
        combine_attributes/2,
	
	'$check_attr'/3,
        '$combine_attr'/4.

%
% called from the emulator
% if there is just one single pending unification
%
uvc(A, B) :- 
%D	display( ['DEBUG: uvc: vars: ' , A , B] ),nl,
	get_attribute( A , AT ),

	%%% Here we mix old and new version
        (
	    AT = simple_attr( L )
	->
            %%% NEW VERSION
	    detach_attribute( A ),
	    A = B,
	    int_verify_attr( L , B )
	;
	    %%% OLD VERSION
	    verify_attribute( AT , B )
	).




ucc(A, B) :-
%D	display( ['DEBUG: ucc: vars: ' , A , B] ),nl,
	get_attribute( A , AT ),
	get_attribute( B , BT ),
	(
	    AT = simple_attr( _ ),
	    BT = simple_attr( _ )
	->
	    %%% NEW VERSION
	    detach_attribute( A ),
	    detach_attribute( B ),
	    A = B,
	    combine_global_attributes( AT , BT , A )
	;
	    %%% OLD VERSION
	    combine_attributes( AT , BT )
	).




% there are more pending unifications (relatively rare)
%
pending_unifications( [] ).
pending_unifications( [ [V1|V2] | Cs ] ) :-
  pending_unification(V1, V2),
  pending_unifications(Cs).

pending_unification(A, B) :-
  (
      get_attribute(A, _)
  ->
      (
	  get_attribute(B, _)
      ->
          ucc(A, B)
      ;
	  uvc(A, B)
      )
  ;
     (
	 get_attribute(B, _)
     ->
         uvc(B, A)
     ;
	 A = B % reduced to syntactic unification
     )
  ).





combine_global_attributes( simple_attr( LA ) , simple_attr( LB ) , Ret ) :-
	!,
%D 	display( 'In combine_global_attributes: ' ) , display( (simple_attr( LA ) , simple_attr( LB )) ), nl ,
	int_combine_attr( LA , LB , Last , Last , Ret ),
%D 	display( 'Last: ' ) , display( Last ) , nl ,
%D 	display( 'LA: ' ) ,display( LA ) , nl ,
%D 	display( 'LB: ' ) ,display( LB ) , nl ,
%D 	display( 'Ret: ' ) , display( Ret ) , nl ,
	(
	    Last == []
	->
	    true
	;
	    (
		get_attribute( Ret , simple_attr( RetAttr ) )
	    ->
		% We have to merge Ret and Last attributes
                recursive_insert( Last , RetAttr , MergedAttr ),
		update_attribute( Ret , simple_attr( MergedAttr ) )
	    ;
		(
		    nonvar( Ret ) ->
		    true
		;
		    % Great! Ret has no attributes!
		    attach_attribute( Ret , simple_attr( Last ) )
		)
	    )
	).


% 	(
% 	  Last == [] ->

% 	    true
%         ;

% 	  nonvar( Ret ) ->

% 	    true
% 	;
%           get_attribute( Ret , simple_attr( RetAttr ) ) ->

% 	    merge_attr( Ret , RetAttr , Last )
%         ;
% 	    attach_attribute( Ret , simple_attr( Last ) )
% 	).








recursive_insert( [] , A , A ).

recursive_insert( [A|As] , L , S ) :- 
	internal_insert_attr( A , L , LA ),
	recursive_insert( As , LA , S ).




%%%
%%% --- From mattr_global.pl

%% Insert in order. If it exists, replace it.

% the same => rewrite
internal_insert_attr( A , [P|R] , [A|R] ) :-
	functor( A , N , _ ),
	functor( P,  N , _ ), 
	!.

% less => insert
internal_insert_attr( A , [P|R] , [A,P|R] ) :-
	A @< P, 
	!.

% keep it
internal_insert_attr( A , [P|R] , [P|LR] ) :-	
	internal_insert_attr( A , R, LR ), 
	!.

% last or empty => add
internal_insert_attr( A , [] ,[A] ).

%%%%






int_combine_attr( [ ] , B , _ , B , _ ) :- !.

int_combine_attr( B , [ ] , _ , B , _ ) :- !.

int_combine_attr( [ A | AR ] , [ B | BR ] , Bef , BefX , Ret ) :-
%D	display( 'Entrada en combine ' ), display( [[A|AR],[B|BR],Bef,BefX] ) , nl,
	!,
	functor( A , FA , _ ),
	functor( B , FB , _ ),
	(
	  FA @< FB 
	->
	  % A belongs to a module before than B => Keep A
	  BefX = [A|Next],
	  int_combine_attr( AR , [B|BR] , Bef , Next , Ret )
	;
	  (
	      FA == FB
	  ->
	      % belongs to the same module
	      local_combine_attr( A , B , SOL ),
	      (
		  nonvar(SOL)
	      ->
%D	          display( 'SOL IS GROUND!' ), nl,
	          Ret = SOL,
		  BefX = [],
%D 		  display( 'Current lists: Bef ' ) , display( Bef ) , nl,
%D 		  display( 'AR: ' ) , display( AR ),nl,
%D 		  display( 'BR: ' ) , display( BR ),nl,
	          int_verify_attr( Bef , SOL ),
		  int_verify_attr( AR  , SOL ),
		  int_verify_attr( BR  , SOL )
	      ;
		  (
		      get_attribute( SOL , simple_attr( [SolAttr] ) )
		  -> 
		      BefX = [ SolAttr | Next ],
		      int_combine_attr( AR , BR , Bef , Next , Ret )
		  ;
		      int_combine_attr( AR , BR , Bef , BefX , Ret )
		  )
	      )
	  ;
	      BefX = [B|Next],
	      int_combine_attr( [A|AR] , BR , Bef , Next , Ret )
	  )
	).




local_combine_attr( A , B , Sol ) :-
	functor( A , Key , _ ),
	arg( 1 , A , XA ),
	arg( 1 , B , XB ),
%D	display( 'Calling to ' ) , display( '$combine_attr'( Key , XA , XB , Sol ) ),
	'$combine_attr'( Key , XA , XB , Sol ), 
	!.




int_verify_attr( [] , _X ).

int_verify_attr( [At|Ats] , Value ) :- 
	functor( At , Key   , _ ),
	arg( 1 , At , Attr ),
	'$check_attr'( Key , Attr , Value ),
	!,
	int_verify_attr( Ats , Value ).





