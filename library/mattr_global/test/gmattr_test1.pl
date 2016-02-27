:- module( gmattr_test1 , _ , [mattr_global] ).
 

:- use_package( show_trans ).


% normal
t1( X , Y ) :- 
	set_attr( X , patata ),
	get_attr( X , Y ).
	
% limitations => overwrite
t2( X , Y ) :- 
	set_attr( X , patata ),
	set_attr( X , patata2 ),
	get_attr( X , Y ).

% ?- main( X , Y ).

% Y = patata,
% attach_attribute(X,simple_attr([f2(patata)])) ? ;

% no
% ?- main2( X , Y ).

% Y = patata2,
% attach_attribute(X,simple_attr([f2(patata2)])) ? ;

% no


%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

