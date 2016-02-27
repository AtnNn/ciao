:- module( ddlist , [null_list/1,
	              next/2,
		      prev/2,
		      insert/3,
		      insert_after/3,
		      delete/2,
		      delete_after/2,
		      top/2,
		      rewind/2,
		      forward/2,
		      length/2,
		      ddlist/1], [assertions,regtypes,isomodes] ).

:- use_module( library(lists) , [length/2] ).

:- comment(title,"Double linked list").
:- comment(author, "David Trallero Mena").

:- comment(module, "This library allows the user to work with double linked
lists. An index is used for referencing the current element in the
list. Such index can be modified by @em{next} and @em{prev} predicated. The
value of the current index can be obtained with @em{top} predicate").


:- regtype ddlist(X)
# "@var{X} is double linked list".

ddlist( (A,B) ) :-
	list(A),
	list(B).

:- pred null_list( ?NullList )
	:  (var( A ))
        => (ddlist( A ))
# "@var{NullList} is an empty list".


null_list( ([],[]) ).

:- pred next( ddlist(OldList) , ddlist(NewList) )
# "@var{NewList} is @var{OldList} but index is set to next element of current element of OldList.\n
It satisfies next(A,B),prev(B,A)".
 
next( (Mirror , [A|R]) , ([A|Mirror] , R) ).

:- pred prev( ddlist(OldList) , ddlist(NewList) )
# "@var{NewList} is @var{OldList} but index is set to previous element of current element of @var{OldList}op) of @var{OldList}".

prev( ([A|Mirror] , R) , (Mirror , [A|R]) ).


:- pred delete( ddlist(OldList) , ddlist(NewList) )
# "@var{NewList} does not have the previous element (top(prev)) of @var{OldList}".

delete( ([_|A],B) , ( A,B) ).
delete( ([_]  ,B) , ([],B) ).

:- pred delete_after( ddlist(OldList) , ddlist(NewList) )
# "@var{NewList} does not have the current element (top) of @var{OldList}".


delete_after( (A,[_|B]) , (A, B) ).
delete_after( (A,[_]  ) , (A,[]) ).


:- pred insert( ddlist(List) , Element , ddlist(NewList) )
# "@var{NewList} is like @var{List} but with @var{Element} inserted _BEFORE_ the current index\n
It satisfies insert( X , A , Xp ) , delete( Xp , X ).".

insert( (A,B), I , ([I|A],B) ).


:- pred insert_after( ddlist(List) , Element , ddlist(NewList) )
# "@var{NewList} is like @var{List} but with @var{Element} inserted _AFTER_ the current index\n
It satisfies insert_after( X , A , Xp ) , delete_after( Xp , X ).".

insert_after( (A,[T|B]), I , (A,[T|[I|B]]) ) :- !.
insert_after( (A,[]), I , (A,[I]) ).


:- pred top( ddlist(List) , Element )
# "@var{Element} is the element pointed by index".

top( (_,[A|_]), A).


:- pred length( ddlist(List) , Len )
# "@var{Len} is the length of the @var{List}".

length( (A,B) , L ) :- 
	lists:length(A,LA), 
	lists:length(B,LB),
	L is LA + LB.


:- pred length_next( ddlist(List) , Len )
# "@var{Len} is the length from the current index till the end".

length_next( (_,B) , L ) :- lists:length( B , L ).


:- pred length_prev( ddlist(List) , Len )
# "@var{Len} is the length from the beginning till the current index".

length_prev( (A,_) , L ) :- lists:length( A , L ).


:- pred rewind( ddlist(OldList) , ddlist(NewList) )
# "@var{NewList} is the @var{OldList} but index is set to 0".

rewind( ([] , A), ([],A) ) :- !.
rewind( ([M|MR] , A), S ) :-
	rewind( (MR,[M|A]) , S ).


:- pred forward( ddlist(OldList) , ddlist(NewList) )
# "@var{NewList} is the @var{OldList} but index is set to lentgh of @var{NewList}".

forward( (A,[]) , (A,[]) ) :- !.
forward( (A,[M|MR]), S ) :-
	forward( ([M|A],MR) , S ).


:- comment(appendix,"

   Two simple examples of the use of the ddlist library package
   follow.  

    @subsection{Using insert_after}

@noindent
@begin{verbatim}
@includeverbatim{examples/ddl1}
@end{verbatim}
 

    @subsection{More Complex example}

@noindent
@begin{verbatim}
@includeverbatim{ddlist/examples/ddl2}
@end{verbatim}

   ").

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+116,2003/12/01,22:04*57+'CET'), "Version 1.0. David
Trallero Mena (David Trallero Mena)").




