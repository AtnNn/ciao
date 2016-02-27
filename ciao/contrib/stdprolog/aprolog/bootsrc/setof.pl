%   File   : SETOF.PL
%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: define setof/3, bagof/3, findall/3,
%   Needs  : Not.Pl

:- module(setof, [findall/3, setof/3, bagof/3]).

:- use_module(err, [err_check/2]).
:- use_module(term, [compare/3, term_variables/2, collect_vars/3]).
:- use_module(lists, [select/3, list_or_partial_list/1]).


/*  This file defines two predicates which act like setof/3 and bagof/3.
    I have seen the code for these routines in Dec-10 and in C-Prolog,
    but I no longer recall it, and this code was independently derived
    in 1982 by me and me alone.

    Most of the complication comes from trying to cope with free variables
    in the Filter; these definitions actually enumerate all the solutions,
    then group together those with the same bindings for the free variables.
    There must be a better way of doing this.  I do not claim any virtue for
    this code other than the virtue of working.  In fact there is a subtle
    bug: if setof/bagof occurs as a data structure in the Generator it will
    be mistaken for a call, and free variables treated wrongly.  Given the
    current nature of Prolog, there is no way of telling a call from a data
    structure, and since nested calls are FAR more likely than use as a
    data structure, we just put up with the latter being wrong.  The same
    applies to negation.

    Would anyone incorporating this in their Prolog system please credit
    both me and David Warren;  he thought up the definitions, and my
    implementation may owe more to subconscious memory of his than I like
    to think.  At least this ought to put a stop to fraudulent claims to
    having bagof, by replacing them with genuine claims.

    Thanks to Dave Bowen for pointing out an amazingly obscure bug: if
    the Template was a variable and the Generator never bound it at all
    you got a very strange answer!  Now fixed, at a price.
*/

%   findall(Template, Goal, List)
%   is a special case of bagof, where all free variables in the
%   generator are taken to be existentially quantified.  It is
%   described in Clocksin & Mellish on p152.  The code they give
%   has a bug (which the Dec-10 bagof and setof predicates share)
%   which this has not.

findall(Template, Goal, List) :-
	callable(Goal),
	list_or_partial_list(List), !,
	save_instances(-Template, Goal),
	list_instances([], List).
findall(Template, Goal, List) :-
	err_check(findall(Template, Goal, List),
		   [inst(Goal),
		    callable(Goal),
		    list(List)
		   ]).

%   setof(Template, Generator, Set)
%   finds the Set of instances of the Template satisfying the Generator..
%   The set is in ascending order (see compare/3 for a definition of
%   this order) without duplicates, and is non-empty.  If there are
%   no solutions, setof fails.  setof may succeed more than one way,
%   binding free variables in the Generator to different values.  This
%   predicate is defined on p51 of the Dec-10 Prolog manual.

setof(Template, Generator, Set) :-
	free_variables(Generator, Goal, Template, Vars),
	callable(Goal),
	list_or_partial_list(Set), !,
	bagof_vars(Vars, Template, Goal, Bag),
	sort(Bag, Set).
setof(Template, Generator, Set) :-
	err_check(setof(Template, Generator, Set),
		   [inst(Generator),
		    do(free_variables(Generator, Goal, Template, _)),
		    inst(Goal),
		    callable(Goal),
		    list(Set)
		   ]).



%   bagof(Template, Generator, Bag)
%   finds all the instances of the Template produced by the Generator,
%   and returns them in the Bag in they order in which they were found.
%   If the Generator contains free variables which are not bound in the
%   Template, it assumes that this is like any other Prolog question
%   and that you want bindings for those variables.  (You can tell it
%   not to bother by using existential quantifiers.)
%   bagof records three things under the key '.':
%	the end-of-bag marker	       -
%	terms with no free variables   -Term
%	terms with free variables   Key-Term
%   The key '.' was chosen on the grounds that most people are unlikely
%   to realise that you can use it at all, another good key might be ''.
%   The original data base is restored after this call, so that setof
%   and bagof can be nested.  If the Generator smashes the data base
%   you are asking for trouble and will probably get it.
%   The second clause is basically just findall, which of course works in
%   the common case when there are no free variables.

bagof(Template, Generator, Bag) :-
	free_variables(Generator, Goal, Template, Vars),
	callable(Goal),
	list_or_partial_list(Bag), !,
	bagof_vars(Vars, Template, Goal, Bag).
bagof(Template, Generator, Bag) :-
	err_check(bagof(Template, Generator, Bag),
		   [inst(Generator),
		    do(free_variables(Generator, Goal, Template, _)),
		    inst(Goal),
		    callable(Goal),
		    list(Bag)
		   ]).

bagof_vars([], Template, Goal, Bag) :- !,
	save_instances(-Template, Goal),
	list_instances([], Bag),
	Bag \= [].
bagof_vars(Vars, Template, Goal, Bag) :-
	Key =.. [.|Vars],
	functor(Key, ., N),
	save_instances(Key-Template, Goal),
	list_instances(Key, N, _, [], OmniumGatherum),
	keysort(OmniumGatherum, Gamut), !,
	concordant_subset(Gamut, Key, Answer),
	Bag = Answer.



%   save_instances(Template, Generator)
%   enumerates all provable instances of the Generator and records the
%   associated Template instances.  Neither argument ends up changed.

save_instances(Template, Generator) :-
	asserta('$bag_instance'(-)),
	call(Generator),
	asserta('$bag_instance'(Template)),
	fail.
save_instances(_, _).


%   list_instances(SoFar, Total)
%   pulls all the -Template instances out of the data base until it
%   hits the - marker, and puts them on the front of the accumulator
%   SoFar.  This routine is used by findall/3-4 and by bagof when
%   the Generator has no free variables.

list_instances(SoFar, Total) :-
	retract('$bag_instance'(Term)),
	!,		%   must not backtrack
	list_instances(Term, SoFar, Total).


list_instances(-, SoFar, Total) :- !,
	Total = SoFar.		%   = delayed in case Total was bound
list_instances(-Template, SoFar, Total) :-
	list_instances([Template|SoFar], Total).



%   list_instances(Key, NVars, Vars, BagIn, BagOut)
%   pulls all the Key-Template instances out of the data base until
%   it hits the - marker.  The Generator should not touch recordx(.,_,_).
%   Note that asserting something into the data base and pulling it out
%   again renames all the variables; to counteract this we use replace_
%   key_variables to put the old variables back.  Fortunately if we
%   bind X=Y, the newer variable will be bound to the older, and the
%   original key variables are guaranteed to be older than the new ones.
%   This replacement must be done @i<before> the keysort.

list_instances(Key, NVars, Vars, OldBag, NewBag) :-
	retract('$bag_instance'(Term)),
	!,		%  must not backtrack!
	list_instances(Term, Key, NVars, Vars, OldBag, NewBag).


list_instances(-, _, _, _,  AnsBag, AnsBag) :- !.
list_instances(NewKey-Term, Key, NVars, Vars, OldBag, NewBag) :-
	replace_key_variables(NVars, Key, Vars, NewKey), !,
	list_instances(Key, NVars, Vars, [NewKey-Term|OldBag], NewBag).



%   There is a bug in the compiled version of arg in Dec-10 Prolog,
%   hence the rather strange code.  Only two calls on arg are needed
%   in Dec-10 interpreted Prolog or C-Prolog.

replace_key_variables(0, _, _, _) :- !.
replace_key_variables(N, OldKey, Vars0, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	replace_variables(Arg, Vars0, Vars1),
	M is N-1,
	replace_key_variables(M, OldKey, Vars1, NewKey).
replace_key_variables(N, OldKey, Vars, NewKey) :-
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
	M is N-1,
	replace_key_variables(M, OldKey, Vars, NewKey).

replace_variables(Term, [Var|Vars], Vars) :-
	var(Term), !,
	Term = Var.
replace_variables(Term, Vars, Vars) :-
	atomic(Term), !.
replace_variables(Term, Vars0, Vars) :-
	functor(Term, _, Arity),
	replace_variables_term(Arity, Term, Vars0, Vars).

replace_variables_term(0, _, Vars, Vars) :- !.
replace_variables_term(N, Term, Vars0, Vars) :-
	arg(N, Term, Arg),
	replace_variables(Arg, Vars0, Vars1),
	N1 is N-1,
	replace_variables_term(N1, Term, Vars1, Vars).
	

%   concordant_subset([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.

concordant_subset([Key-Val|Rest], Clavis, Answer) :-
	concordant_subset(Rest, Key, List, More),
	concordant_subset(More, Key, [Val|List], Clavis, Answer).


%   concordant_subset(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.

concordant_subset([Key-Val|Rest], Clavis, [Val|List], More) :-
	Key == Clavis, !,
	concordant_subset(Rest, Clavis, List, More).
concordant_subset(More, _, [], More).


%   concordant_subset/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.

concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
	concordant_subset(More, Clavis, Answer).


%   In order to handle variables properly, we have to find all the 
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
%	a)  they occur in the template
%	b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList)
%   finds this set, using OldList as an accumulator.

free_variables(Generator, Goal, Template, Vars) :-
	nonvar(Generator),
	term_variables(Template, Bound0),
	get_bound(Generator, Goal, Bound0, Bound),
	term_variables(Goal, AllVars),
	free_variables_in(AllVars, Bound, Vars).

get_bound(V^Generator, Goal, Bound0, Bound) :- !,
	nonvar(Generator),
	collect_vars(V, Bound1, Bound0),
	get_bound(Generator, Goal, Bound1, Bound).
get_bound(Goal, Goal, Bound, Bound).

free_variables_in([], _, []).
free_variables_in([V|L], Bound, Free) :-
	(    \+ var_memberchk(V, Bound) -> Free = [V|Free1]
        ;    Free = Free1
        ),
	free_variables_in(L, Bound, Free1).

var_memberchk(V, [X|_]) :- X == V, !.
var_memberchk(V, [_|Xs]) :- var_memberchk(V, Xs).


/*  Note: these definitions are to be read as specifications of what
    arguments are acceptable and what results should be obtained.
    They are NOT to be read as being the code which should actually
    be used.  I have C versions of these routines which are efficient
    with no redundant storage turnover and which make using the general
    routines nearly as fast as using the specific versions would be.
    The C code can also exploit any existing order or reversed order.
    Since keysort/2 and sort/2 are built into Dec-10 Prolog:
*/

sort(_Key, _Order, [], []).
sort(_Key, _Order, [X], [X]).
sort(Key, Order, [X,Y|L], Sorted) :-
	halve(L, [Y|L], Front, Back),
	sort(Key, Order, [X|Front], F),
	sort(Key, Order, Back, B),
	merge(Key, Order, F, B, Sorted).


halve([_,_|Count], [H|T], [H|F], B) :- !,
	halve(Count, T, F, B).
halve(_, B, [], B).


merge(Key, Order, [H1|T1], [H2|T2], [Hm|Tm]) :- !,
	compare(Key, Order, H1, H2, R),
	(   R = (<) -> Hm = H1, merge(Key, Order, T1, [H2|T2], Tm)
	;   R = (>) -> Hm = H2, merge(Key, Order, [H1|T1], T2, Tm)
	;   R = (=) -> Hm = H1, merge(Key, Order, T1, T2, Tm)
	).
merge(_, _, [], L, L) :- !.
merge(_, _, L, [], L).


compare(Key, Order, X, Y, R) :-
	compare(Key, X, Y, R0),
	combine(Order, R0, R).

compare(0, X, Y, R) :- !,
	compare(R, X, Y).
compare(N, X, Y, R) :-
	arg(N, X, Xn),
	arg(N, Y, Yn),
	compare(R, Xn, Yn).


combine(<, R, R).
combine(=<, >, >) :- !.
combine(=<, _, <).
combine(>=, <, >) :- !.
combine(>=, _, <).
combine(>, <, >) :- !.
combine(>, >, <) :- !.
combine(>, =, =).


keysort(R, S) :-
	sort(1, =<, R, S).


sort(R, S) :-
	sort(0, <, R, S).

