:- module( mattr_sicstus_code, [
	                    put_atts/3,
			    get_atts/3
			   ] , [ ] ).

:- use_module(library(odd)).

:- use_module(library(mattr_global(mattr_global_code))).

:- multifile '$attr_hash$'/3.
:- multifile '$max_num_attr$'/2.

:- data global_var/3.

put_atts( A , B , M ) :-
	put_atts_bd( A , M , B ).
	

put_atts_bd( _ , _ , L ) :-
	nonvar( L ),
	L = [],
	!.
put_atts_bd( X , M , L ) :-
	nonvar( L ),
	list(L),
	L = [A|As],
	!,
	put_atts_bd( X , M , A ),
	put_atts_bd( X , M , As ).
put_atts_bd( X , M , '-'(Y) ) :-
	!,
	detach_attr( X , M , Y ).
put_atts_bd( X , M , '+'(Y) ) :-
	!,
	put_atts_bd( X , M , Y ).
put_atts_bd( X , M , Y ) :-
	functor( Y , F , Ar ),
	'$attr_hash$'( M , f(F,Ar1) , Place ),
	( Ar1 == no -> true ; Ar1 = Ar ),
	(
	    get_attr( X , Attr , M )
	->
	    setarg( Place , Attr , Y ),
	    set_attr( X , Attr , M )
	;
	    '$max_num_attr$'( M , L ),
	    L1 is L+1,
	    functor( Attr , sicstus_attr , L1 ),
	    setarg( Place , Attr , Y ),
	    set_attr( X , Attr , M )
	),
	!.


detach_attr( X , M , Attr ) :-
	functor( Attr , F , Ar ),
	'$attr_hash$'( M , f(F,Ar1) , Place ),
	( Ar1 == no -> true ; Ar1 = Ar ),
	% Modify the General attribute
	get_attr( X , GenAttr , M ),
	setarg( Place , GenAttr , _ ),
	GenAttr =.. [ _ | Args ],
	( list( Args , var ) ->
	     mattr_global_code:detach_attr( X , M )
	  ;
	     set_attr( X , GenAttr , M )
	).


% ----------------------------------------------------------------------------

get_atts( A , B , M ) :-
	get_atts_bd( A , M , B ).




% We have to return a list with all attributes
get_atts_bd( X , M , Var ) :-
	var( Var ),
	!,
	'$max_num_attr$'( M , L ),
	(
	    get_attr( X , Attr , M ) 
	->
	    L1 is L + 2,
	    get_atts_list( Attr , 2 , L1 , Var )
	;
	    Var = []
	).
get_atts_bd( X , M , '+'(Y) ) :-
	!,
	get_attr( X , Attr , M ),
	functor( Y , F , Ar ),
	'$attr_hash$'( M , f(F,Ar1) , Place ),
	( Ar1 == no -> true ; Ar1 = Ar ),
	arg( Place , Attr , Y1 ),
	nonvar( Y1 ),
	Y=Y1.
% IF the variable has not attributes it has to succeed!!
get_atts_bd( X , M , '-'(Y) ) :-
	!,
	(
	    get_attr( X , Attr , M )
	->
  	    functor( Y , F , Ar ),
	    '$attr_hash$'( M , f(F,Ar1) , Place ),
	    ( Ar1 == no -> true ; Ar1 = Ar ),
	    arg( Place , Attr , Y1 ),
	    var( Y1 )
	;
	    true
	).
get_atts_bd( X , M , Y ) :-
	get_attr( X , Attr , M ),
	functor( Y , F , Ar ),
	'$attr_hash$'( M , f(F,Ar1) , Place ),
	( Ar1 == no -> true ; Ar1 = Ar ),
	arg( Place , Attr , Y ),
	!.



get_atts_list( _ , B , B , [] ) :-
	!.
get_atts_list( Attr , B , E , V ) :-
	arg( B , Attr , A ),
	B1 is B + 1,
	get_atts_list( Attr , B1 , E , Vs ),
	(
	    var( A )
	->  
	    V = Vs
	;
	    V = [A|Vs]
	).
