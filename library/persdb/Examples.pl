:- use_package([assertions]).

:- comment(title,"Using the persdb library").

:- comment(author,"The CLIP Group").

:- comment(module,"Through the following examples we will try to
	illustrate the two mains ways of declaring and using
	persistent predicates: statically (the preferred method) and
	dynamically (necessary when the new persistent predicates have
	to be defined at run-time). The final example is a small
	application implementing a simple persistent queue.

        @comment{main applications of the persistent predicates. These
	are using persistent predicates from a prolog @concept{top level}
	and using them in @concept{standalone programs}. Both share 
	the same base which is achieving persistency in a straight forward,
	transparent way but with a subtle difference, which is the update
	of the persistence set. 

        In a standalone program the persistence set is updated each time
        the program is run but, in a top level it only happens whenever 
        the top level is started. This is due to the fact of the top level
        being the executable program and not the program itself. In fact, 
	this is the most logical way to do it because as long as there is 
        a top level holding the state of the persistent predicates, there 
        is no need to waste machine resources making updates. 

        Anyway, if a program launched from a top level needs to update 
        the persistence sets of any persistent predicate it can be done 
        by using the methods @pred{update_files/0} and @pred{update_files/1}.}

@section{An example of persistent predicates (static version)}

@begin{verbatim}
@includeverbatim{example_static.pl}
@end{verbatim}

@section{An example of persistent predicates (dynamic version)}

@begin{verbatim}
@includeverbatim{example_dynamic.pl}
@end{verbatim}

@section{A simple application / a persistent queue}
@begin{verbatim}
@includeverbatim{queue.pl}
@end{verbatim}

").

main.

%% *** Delete this comment after reading: it is only a reminder! ***
%% 
%% The "assertions" library needs to be included in order to support
%% ":- comment(...,...)." declarations such as below, i.e., insert: 
%% 
%% :- module(_,_,[assertions]).
%% 
%% At the beginning of the file:
%% The following version comment(s) can be moved elsewhere in the 
%% file. Subsequent version comments will always be placed above 
%% the last one inserted.


:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*11+108,2003/12/22,18:03*39+'CET'), "Added comment
   author.  (Edison Mera)").

