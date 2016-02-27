:- module( mattr_global , [ 
	                         get_attr/2 , set_attr/2 , detach_attr/1,
				 
	                         % Back doors for local_attributes
			         bd_get_attr/3 , bd_set_attr/3
			       ] , [] ).

:- use_module( engine( attributes ) ).

:- use_module( library(lists)).

:- meta_predicate set_attr(    ? , addmodule ).
:- meta_predicate get_attr(    ? , addmodule ).
:- meta_predicate detach_attr( addmodule ).


% BACK DOOR versions
bd_set_attr(    A , B , C ) :- set_attr( A , B , C ).
bd_get_attr(    A , B , C ) :- get_attr( A , B , C ).
%bd_update_attr( A , B , C ) :- update_attr( A , B , C ).

%get_attr
get_attr( X , A , user(_) ) :-
	!,
	get_attr( X , A , user ).

get_attr( X , ModuleAttr , Name ) :-
	attributes:get_attribute( X , simple_attr( Attr ) ),
	Find =.. [ Name , ModuleAttr ],
	member( Find , Attr ).



%% Insert in order. If it exists, replace it
% the same => rewrite
internal_insert_attr( A , [P|R] , [A|R] ) :-
	functor( A , N , _ ),
	functor( P,  N , _ ), 
	!.

% less => insert
internal_insert_attr( A , [P|R] , [A,P|R] ) :-
	A @< P, 
	!.

internal_insert_attr( A , [P|R] , [P|LR] ) :-	
	internal_insert_attr( A , R, LR ), 
	!.

% last or empty => add
internal_insert_attr( A , [] ,[A] ).


%% Delete in order.

internal_delete_attr( A , [P|R] , RES ) :-
	functor( A , FA , _ ),
	functor( P,  FP , _ ), 
	(
	    FA @< FP
	->
            % less => continue
            RES = [P|RET],
	    internal_delete_attr( A , R , RET )
	;
	    % the same => delete it
	    FA == FP,
	    RES = R
	),
	!.


% A @> P or last or empty => it was no here
internal_delete_attr( _ , A , A ).


% Things that can happend:
% 1. Module 'Name has never set an attribute to variable X
% 2. Attribute is locked, so 2nd arg has to be changed
% 3. Attribute is no locked, so 1st arg has to be changed

set_attr( X , A , user(_) ) :-
	!,
	set_attr( X , A , user ).
	

set_attr( X , A , Name ) :-
	(
	    attributes:get_attribute( X , simple_attr( Attr ) )
	->    
	    NA =.. [Name , A],
            internal_insert_attr( NA , Attr , New_AttList ),
	    attributes:update_attribute( X , simple_attr( New_AttList ) )
	;
	    % It is the first time that module 'Name' sets 
	    % an attribute to this variable
	    NA =.. [Name , A ],
	    (
	      var(X) ->
	      attributes:attach_attribute( X , simple_attr( [NA] ) )
	    ;
              message( error , ['set_attr: attaching ' , A , ' to nonvar ' , X ])
	    )
	).




detach_attr( X , user(_) ) :-
	!,
	detach_attr( X , user ).

detach_attr( X , Name ) :-
	attributes:get_attribute( X , simple_attr( Attr ) ),
	NA =.. [Name , _ ],
	internal_delete_attr( NA , Attr , New_AttList ),
	attributes:update_attribute( X , 
	                             simple_attr( New_AttList ) ).


