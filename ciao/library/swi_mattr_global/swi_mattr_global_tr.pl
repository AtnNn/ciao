:- module( _ , [ change_atts/2 , add_atts_code/3 ] , [] ).


% change_atts( put_attr( A , M , C ) , set_attr( A , C , M ) ).
% change_atts( get_attr( A , M , C ) , mattr_global_code:get_attr( A , C , M ) ).
% change_atts( del_attr( A , M )     , detach_attr( A , M )  ).

change_atts( get_attr( A , _M , C ) , get_attr( A , C ) ).
change_atts( put_attr( A , _M , C ) , set_attr( A , C ) ).
change_atts( del_attr( A , _M )     , detach_attr( A )  ).

% This is a SWI built-in
change_atts( attvar( X ), get_attr(X,_) ).

add_atts_code( end_of_file , 
	       [ 
		   (combine_attr( A , B , X ) :-
%		    display( hook1 ),nl,
		    set_attr( X , B ),
		    attr_unify_hook( A , X )),

%		    (:-use_module( library( write ) )),
		    
		   (check_attr( A , X ) :-
%		   display( hook2(_M,X) ),nl,
%		   write_term(A,[max_depth(8)]),nl,
		    attr_unify_hook( A , X )),

		   end_of_file 
	       ] ,
	       _M ).

