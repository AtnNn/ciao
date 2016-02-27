:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title,"Andorra execution").  

:- comment(author, "Claudio Vaucheret").
:- comment(author, "Francisco Bueno").

:- comment(module,

"This package allows modules execution under the Basic Andorra Model
@cite{andorra-principle}. The model classifies goals as
@em{determinate}, if at most one clause matches the goal, or
nondeterminate, otherwise. In this model a goal is delayed until
either it becomes determinate or it becomes the leftmost goal and no
determinate goal is available. The implementation of this selection
rule is based on the use of attributed variables
@cite{holzbaur-plilp92,holzbaur-phd}.

In order to test determinacy, we verify only the heads of clauses and
builtins in the bodies of clauses before the first cut.  By default,
when a goal is determinate, is detected dynamically, regarding the non
matching clauses each time a variable appearing in the goal is
instantiated.

In addition, efficiency can be improved by user directives setting the
determinacy conditions.

The directive:

@tt{:- determinate(Pred,Cond)}

states than the predicate @var{Pred} is determinate when @var{Cond}
holds.  @var{Cond} can be a single condition or conjunctions and
disjunctions of conditions. A single condition can be:

@begin{enumerate} 
@item @tt{ground(A)} where A is an argument of @var{Pred} 
@item @tt{nonvar(A)} where A is an argument of @var{Pred}
@item @tt{instatiated(A,Path)} which means that the subterm of @tt{A}
addressed by @tt{Path} is not var.  @tt{A} is an argument of
@var{Pred} and @tt{Path} is a list of integer numbers describing a
path to the subterm regarding the whole term A as a tree. For example,
@tt{instantiated(f(g(X),h(i(Z),Y)),[2,1])} tests whether @tt{i(Z)} is
not var.  
@item @tt{Term1 ?\\= Term2} which means ``if the terms Term1
and Term2 are instantiated they do not unify''. @tt{Term1} and
@tt{Term2} can be either any argument of @tt{Pred} or a term
@tt{term(V,Path)} which refers to the subterm of @tt{V} addressed by @tt{Path}. 
@item @tt{Term1 ?= Term2} which means ``if the terms Term1
and Term2 are instantiated they unify''
@item any test that does not unify variables, ( @tt{==/2}, @tt{\\==/2}, @tt{atomic/1}).
@end{enumerate}

For example 
@begin{verbatim}
:- determinate(member(A,B,C), ( A ?\= term(B,[1])  ; C?\\=[_|_]) ).

member(A,[A|B],B).
member(A,[B|C],[B|D]) :-
        A\==B,
        member(A,C,D).
@end{verbatim}

In this example, the directive states than the call @tt{member(A,B,C)} is determinate when @tt{A} doesn't unify to the first argument of @tt{B} or @tt{C} doesn't unify to @tt{[_|_]}.


It is possible to preserve the computation rule for calls to
predicates defined in other modules. These modules should obviously also
use this package. In addition @em{all} predicates from such modules should
imported, i.e., the directive @tt{:- use_module(module)}, should be used in
this case instead of @tt{:- use_module(module,[...])}.  Otherwise calls to
predicates outside the module will only be called  when they became the leftmost goal.


").


:- include(library('andorra/andorraops')).







