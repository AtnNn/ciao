:- use_package(assertions).
:- comment(nodoc,assertions).

:- comment(title,"Fuzzy Prolog").  

:- comment(author, "Claudio Vaucheret").
:- comment(author, "Sergio Guadarrama").

:- comment(module, "This package impements an extension of prolog to
deal with uncertainty. We implement a fuzzy prolog that models
interval-valued Fuzzy logic. This approach is more general than others
Fuzzy Prolog in two aspects:

@begin{enumerate}
@item Truth value will be a sub-interval on [0,1]. In fact, it could
  be a finite union of sub-intervals as we will see below. Having a
  unique truth value is a particular case modeled with a unitary
  interval.
@item Truth value will be propagated through the rules by means of a
  set of @em{aggregation operators}. The definition of an @em{aggregation
  operator} is a generalization that subsumes conjunctive operators
  (triangular norms as min, prod, etc), disjunctive operators
  (triangular co-norms as max, sum, etc), average operators
  (averages as arithmetic average, cuasi-linear  average, etc) and
  hybrid operators (combinations of previous operators).
@end{enumerate}

We add uncertainty using CLP(R) instead of implementing a new fuzzy
resolution as other fuzzy prologs. In this way, we use the original
inference mechanism of Prolog, and we use the constraints and its
operations provided by CLP(R) to handle the concept of partial
truth. We represent intervals as constrains over real numbers and
@emph{aggregation operators} as operations with constraints.

Each fuzzy predicate has an additional argument which represents its
truth value. We use ``:~'' instead of ``:-'' to distinguish fuzzy
clauses from prolog clauses. We have implemented the following
aggregation operators:

@begin{enumerate}
     @item @tt{min} minimum T-norm.
     @item @tt{prod} product T-norm.
     @item @tt{luka} Lukasiewicz T-norm.
     @item @tt{max} maximum T-conomr.
     @item @tt{dprod} product T-conomr.
     @item @tt{dluka} Lukasiewicz T-conorm.
@end{enumerate}

An example of code can be:
@begin{verbatim}
@includeverbatim{examples/young2.pl}
@end{verbatim}

@begin{verbatim}
?- young_couple(john,rose,M).

.=.(M,0.6) ? 

yes
?- 
@end{verbatim}

Fuzzy predicates with piecewise linear continous membership functions like young in the example above is translated to aritmetic constraints, the translation is the following:

@begin{verbatim}
young(X,1):- X .>=. 0, X .<. 35.
young(X,M):- X .>=. 35, X .<. 45, 10*M .=. 45-X.
young(X,0):- X .>=. 45, X .=<. 120. 
@end{verbatim}
 
It is possible to fuzzify crisp predicates. For example to fuzzify
p/2 it is only necessary to write:

@begin{verbatim}
p_f :# fuzzy p/2
@end{verbatim}

and the program is expanded with a new fuzzy predicate p_f/3 (the last
argument is the truth value) with truth value equal to 0 if p/2 fail
and 1 otherwise.

We provide too the possibility of having the predicate that is the
fuzzy negation of a fuzzy predicate. For this predicate p_f/3 we will
define a new fuzzy predicate called for example notp_f/3 with the
following line:

@begin{verbatim}
notp_f :# fnot  p_f/3
@end{verbatim}

that is expanded at compilation time as:

@begin{verbatim}
notp_f(X,Y,M) :-
        p_f(X,Y,Mp),
        M .=. 1 - Mp.
@end{verbatim}

Another example is:

@begin{verbatim}
@includeverbatim{examples/dicesum5.pl}
@end{verbatim}

").



:- include(library('fuzzy/ops')).




