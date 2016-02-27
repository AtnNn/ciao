:- module( mattributes , [] , [] ).

:- use_module( 'matr_local').
:- use_module( library(lists) ).

:- data attr_convert/4.


attr_convert( [] , 0 , false , _ ).

mattr_def((:- attributes(R)), _, M) :-
	length( R , L ),
        asserta_fact(attr_convert( R , L , true , M)).
        

mattr_def(end_of_file, end_of_file, M) :-
        retractall_fact(attr_convert(_,_,_,M)).


mattr_def( get_attr(X,A), get_attr(X,A,I) , M) :-
	get_index( A , I , M ).

get_index( A , I , M ) :-
	attr_convert( L , _ , _ , M ),
	get_position( A , L , I ).

get_index( A , I , M ) :-
	attr_convert( List , Len , true , M ),
	retract_fact( attr_convert( List , Len , M ) ),
	L1 is Len + 1 ,
	List1 = [ A | List ],
	asserta_fact( attr_convert( List2 , L1 , M ) ).

get_index( A , -1 , M ) :-
	user_inform( ['ERROR: Atribute not defined',A,' in module ' , M ] ).

parse_attr_list_decl( LIST ) :-
	compose_local_attr( LIST , DECL , Facts ),
	
