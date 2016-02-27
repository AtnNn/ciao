:- module(native_props,
        [  linear/1
         , mshare/1
         , fails/1
         , not_fails/1
         , possibly_fails/1
         , covered/1
         , not_covered/1 
         , is_det/1
         , possibly_nondet/1
         , mut_exclusive/1
         , not_mut_exclusive/1
         , size_lb/2
         , size_ub/2
         , steps_lb/2 
         , steps_ub/2  
	 , sideff_pure/1
	 , sideff_soft/1
	 , sideff_hard/1
        ],
        [assertions]).

:- reexport(library(andprolog),[indep/1,indep/2]).
:- comment(doinclude,indep/1).
:- comment(doinclude,indep/2).

:- use_module(library(metaterms),[varsbag/3]).
:- use_module(library(sort),[sort/2]).
:- use_module(library(lists)).

% --------------------------------------------------------------------------
:- comment(title,"Properties which are native to analyzers").

:- comment(author,"Francisco Bueno").
:- comment(author,"Manuel Hermenegildo").
:- comment(author,"Pedro Lopez").

:- comment(module,"@cindex{properties, native} This library contains a
   set of properties which are natively understood by the different program
   analyzers of @apl{ciaopp}.  They are used by @apl{ciaopp} on output
   and they can also be used as properties in assertions.").

:- comment(usage,"@tt{:- use_module(library('assertions/native_props'))}

   or also as a package @tt{:- use_package(nativeprops)}.

   Note the different names of the library and the package.").

:- comment(linear(X), "@var{X} is bound to a term which is linear,
   i.e., if it contains any variables, such variables appear only once
   in the term. For example, @tt{[1,2,3]} and @tt{f(A,B)} are linear
   terms, while @tt{f(A,A)} is not.").

:- prop linear(X)
# "@var{X} is instantiated to a linear term.".

linear(T):-
	varsbag(T,VarsBag,[]),
	sort(VarsBag,VarsSet),
	length(VarsBag,N),
	length(VarsSet,N).

:- comment(mshare(X), "@var{X} contains all @index{sharing sets}
   @cite{jacobs88,abs-int-naclp89} which specify the possible variable
   occurrences in the terms to which the variables involved in the
   clause may be bound. Sharing sets are a compact way of representing
   groundness of variables and dependencies between variables. This
   representation is however generally difficult to read for
   humans. For this reason, this information is often translated to
   @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties,
   which are easier to read.").

:- prop mshare(X) 
# "The sharing pattern is @tt{@var{X}}.".

:- impl_defined(mshare/1).

:- comment(fails(X), "Calls of the form @var{X} fail.").

:- prop fails(X)
# "Calls of the form @var{X} fail.".

:- impl_defined(fails/1).

:- comment(not_fails(X), "Calls of the form @var{X} produce at least
  one solution, or not terminate @cite{non-failure-iclp97}.").

:- prop not_fails(X)
# "All the calls of the form @var{X} do not fail.".

:- impl_defined(not_fails/1).

:- comment(possibly_fails(X), "Non-failure is not ensured for any call
of the form @var{X} @cite{non-failure-iclp97}. In other words, nothing
can be ensured about non-failure nor termination of such calls.").

:- prop possibly_fails(X)
# "Non-failure is not ensured for calls of the form @var{X}.".

:- impl_defined(possibly_fails/1).

:- comment(covered(X), "For any call of the form @var{X} there is at
least one clause whose test succeeds (i.e. all the calls of the form
@var{X} are covered.) @cite{non-failure-iclp97}.").

:- prop covered(X) 
# "All the calls of the form @var{X} are covered.".

:- impl_defined(covered/1).

:- comment(not_covered(X), "There is some call of the form @var{X} for
which there is not any clause whose test succeeds
@cite{non-failure-iclp97}.").

:- prop not_covered(X) 
# "Not all of the calls of the form @var{X} are covered.".

:- impl_defined(not_covered/1).

:- comment(is_det(X), "All calls of the form @var{X} are
deterministic, i.e. produce at most one solution, or not terminate.").

:- prop is_det(X)
# "All calls of the form @var{X} are deterministic.".

:- impl_defined(is_det/1).

:- comment(possibly_nondet(X), "Non-determinism is not ensured for all
calls of the form @var{X}. In other words, nothing can be ensured
about determinacy nor termination of such calls.").

:- prop possibly_nondet(X)
# "Non-determinism is not ensured for calls of the form @var{X}.".

:- impl_defined(possibly_nondet/1).

%% disjoint(X)
%% # "Calls of the form @var{X} select at most one clause.".

:- comment(mut_exclusive(X), "For any call of the form @var{X} at most one
clause succeeds, i.e. clauses are pairwise exclusive.").

:- prop mut_exclusive(X)
# "For any call of the form @var{X} at most one clause succeeds.".

:- impl_defined(mut_exclusive/1).

:- comment(not_mut_exclusive(X), "Not for all calls of the form @var{X} at
most one clause succeeds. I.e. clauses are not disjoint for some
call.").

 %% For any call of the form @var{X} at most one
 %% clause succeeds, i.e. clauses are pairwise exclusive.").

:- prop not_mut_exclusive(X)
# "Not for all calls of the form @var{X} at most one clause
  succeeds.".

:- impl_defined(not_mut_exclusive/1).

:- comment(size_lb(X, Y), "The minimum size of the terms to which
the argument @var{Y} is bound to is given by the expression
@var{Y}. Various measures can be used to determine the size of an
argument, e.g., list-length, term-size, term-depth, integer-value,
etc. @cite{caslog}.").

:- prop size_lb(X,Y)
# "@var{Y} is a lower bound on the size of argument @var{X}.".

:- impl_defined(size_lb/2).

:- comment(size_ub(X, Y), "The maximum size of the terms to which
the argument @var{Y} is bound to is given by the expression
@var{Y}. Various measures can be used to determine the size of an
argument, e.g., list-length, term-size, term-depth, integer-value,
etc. @cite{caslog}.").

:- prop size_ub(X,Y)
# "@var{Y} is a upper bound on the size of argument @var{X}.".

:- impl_defined(size_ub/2).

%% upper_size(X,Y)
%% # "The maximum size of arguments of calls of the form @var{X} are
%%    given by the expression @var{Y}.".

:- comment(steps_lb(X, Y), "The minimum computation time (in
resolution steps) spent by any call of the form @var{X} is given by
the expression @var{Y} @cite{low-bounds-ilps97,granularity-jsc}").

:- prop steps_lb(X,Y) 
# "@var{Y} is a lower bound on the cost of any call of the form
@var{X}.".

:- impl_defined(steps_lb/2).

%% lower_time(X,Y)
%% # "The minimum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".

:- comment(steps_ub(X, Y), "The maximum computation time (in
resolution steps) spent by any call of the form @var{X} is given by
the expression @var{Y} @cite{caslog,granularity-jsc}").

:- prop steps_ub(X,Y) 
# "@var{Y} is a upper bound on the cost of any call of the form
@var{X}.".

:- impl_defined(steps_ub/2).

%% upper_time(X,Y)
%% # "The maximum computation time spent by calls of the form @var{X} is
%%    given by the expression @var{Y}.".

:- prop sideff_pure(X) 
# "@var{X} is pure, i.e., has no side-effects.".

:- impl_defined(sideff_pure/1).

:- prop sideff_soft(X) 
# "@var{X} has @index{soft side-effects}, i.e., those not affecting
   program execution (e.g., input/output).".

:- impl_defined(sideff_soft/1).

:- prop sideff_hard(X) 
# "@var{X} has @index{hard side-effects}, i.e., those that might affect
   program execution (e.g., assert/retract).".

:- impl_defined(sideff_hard/1).

% --------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+1,1999/11/29,17:12*34+'MET'), "Changed names of
   the native properties.  (Francisco Bueno Carrillo)").

% --------------------------------------------------------------------------
