:-module( test_local_4 , [p0/2, p/2,p2/2,p3/2,p4/2] , [mattr_global, mattr_local] ).

% OUTPUT:
% ERROR: Attribute 'patata/0' defined in module 'test_local_4' with different arity, check it out: 'patata/1'
% ERROR: Attribute 'patata/0' defined in module 'test_local_4' with different arity, check it out: 'patata/1'
% ERROR: Attribute 'patata/4' defined in module 'test_local_4' with different arity, check it out: 'patata/1'
% ERROR: Attribute 'patata/4' defined in module 'test_local_4' with different arity, check it out: 'patata/1'



:- attribute  patata/1 ,manzana.


p0( X,Y ) :- 
	set_attr( X , patata ),
	get_attr( X , Y ).

p( X,Y ) :- 
	set_attr( X , patata ),
	set_attr( X , manzana ),
	get_attr( X , Y ).




p2( X,Y ) :- 
	set_attr( X , manzana ),
	set_attr( X , patata(12) ),
	get_attr( X , patata(Y) ).

p3( X,Y ) :- 
	set_attr( X , patata(1,2,3,4) ),
	set_attr( X , manzana(5,_) ),
	get_attr( X , Y ).

p4( X,Y ) :- 
	set_attr( X , patata(1,2,3,4) ),
	set_attr( X , patata(1) ),
	get_attr( X , Y ).









