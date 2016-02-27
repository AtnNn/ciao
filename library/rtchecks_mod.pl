:- module( rtchecks_mod , [
	                 checkc/1,
			 checkc/2,
			 checkiftrue/2,
			 rt_det_ncp/1,
			 rt_det_1c/1,
			 rt_nf/1,
			 rt_print/1
		      ] , [] ).


:- use_module( engine( internals   ) ).
:- use_module( engine( terms_check ) ).


%       det   nf
% det      1   1
% semidet  1   0
% multidet 0   1
% nondet   0   0
%
% throw( rtcheck( det_nf , MSG , GOAL  ) )  MSG == notdet ; MSG == fail
% throw( rtcheck( pre    , PRE , GOAL  ) )
% throw( rtcheck( post   , PRE , GOAL  ) )



:- meta_predicate checkc( goal , ? ).
:- meta_predicate checkc( goal ).
:- meta_predicate checkiftrue( ? , goal ).

:- meta_predicate rt_det_ncp( goal ).
:- meta_predicate rt_nf(      goal ).
:- meta_predicate rt_det_1c(  goal ).


:- set_prolog_flag( multi_arity_warnings, off ).

% checkc( A , true ) :-
% 	\+ \+ A,
% 	!.
%
checkc( A , true ) :-
	copy_term( A , AC ),
	AC,
	instance( A , AC ),
	!.

checkc( _A , false ).




% checkc( A ) :-
% 	\+ \+ A,
% 	!.
%
checkc( A , true ) :-
	copy_term( A , AC ),
	AC,
	instance( A , AC ),
	!.

checkc( A ) :- 
	throw( rtcheck( pre , A , true ) ).

:- set_prolog_flag( multi_arity_warnings, on ).




checkiftrue( false , _ ).
checkiftrue( true  , A ) :- 
	(
%	    \+ \+ A
	    copy_term( A , AC ),
	    AC,
	    instance( A , AC )
	-> 
	    true
	;
	    throw( rtcheck( post , A , true ) )
	).




%:- meta_predicate rt_det_nf( goal ).
%% it is like rt_det( rt_nf ) == rt_nf( rt_det )
% rt_det_nf(X) :- 
% 	'$metachoice'( C0 ),
% 	X,
% 	'$metachoice'( C1 ),
% 	( C1 == C0 -> ! ; rtcheck( det_nf , nodet , X  ) ).
%
% rt_det_nf(X) :- throw( rtcheck( det_nf , fail , X  ) ).

%rt_det_nf(X) :- rt_det( rt_nf( X ) ).


% det Not Choice Points
rt_det_ncp(X) :- 
	'$metachoice'( C0 ),
	X,
	'$metachoice'( C1 ),
	( C1 == C0 -> true ; throw( rtcheck( det , nodet , X  ) ) ).




rt_nf(X) :- 
	'$metachoice'( C0 ),
	X,
	'$metachoice'( C1 ),
	( C1 == C0 -> ! ; true ).

rt_nf(X) :- throw( rtcheck( nf , fail , X  ) ).


% no choice point after returning from predicate are allowed
rt_det_1c(X) :-
	Solved = solved(no),
	X,
	( arg(1, Solved, no) -> 
	    true
	; throw( rtcheck( det , nodet , X  )) % more than one solution!
	),
        % Update without trailing: be careful!
        % (in this case, the operation is safe because we are 
        % writting one-cell terms without dereferencing 
        % chains)
	'$setarg'(1, Solved, yes, off). 




rt_print( rtcheck( det , nodet , X  ) ) :-
	message( error , [ 'Goal ' , X , ' was suppoused to be deterministic' ] ).

rt_print( rtcheck( nf  , fail  , X  ) ) :-
	message( error , [ 'Goal ' , X , ' was suppoused not to fail' ] ).

rt_print( rtcheck( pre , PRE  , _X  ) ) :-
	message( error , [ 'Precondition ' , PRE , ' failed' ] ).

rt_print( rtcheck( post , POST  , _X  ) ) :-
	message( error , [ 'Postcondition ' , POST , ' failed' ] ).

% q(a).
% q(b).

% r(_,a).
% r(_,b).

% p(1).
