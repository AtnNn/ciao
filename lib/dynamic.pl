:- module(dynamic, [
        asserta/1, asserta/2, assertz/1, assertz/2, assert/1, assert/2,
        retract/1, retractall/1, abolish/1,
        clause/2, clause/3, current_predicate/1, current_predicate/2,
        dynamic/1, data/1, wellformed_body/3
        ],[assertions,isomodes]).

:- use_module(engine(internals)).

:- meta_predicate
        asserta(clause),
        asserta(clause, ?),
        assertz(clause),
        assertz(clause, ?),
        assert(clause),
        assert(clause, ?),
        retract(clause),
        retractall(fact),
        abolish(spec),
	dynamic(addmodule),
        data(addmodule),
        clause(fact,?),
        clause(fact,?,?),
        current_predicate(addmodule).

:- comment(title,"Dynamic predicates").

:- comment(module,"This module implements the assert/retract family of
   predicates to manipulate dynamic predicates.

   The predicates defined in this module allow modification of the
   program as it is actually running.  Clauses can be added to the
   program (@em{asserted}) or removed from the program (@em{retracted}).
   For these predicates, the argument which corresponds to the clause
   head must be instantiated to an atom or a compound term. The argument
   corresponding to the clause must be instantiated either to a term
   @tt{Head :- Body} or, if the body part is empty, to @tt{Head}. An
   empty body part is represented as @tt{true}.  Note that using this
   library is very detrimental to global analysis, and that for most
   uses the predicates listed in @ref{Fast/concurrent update of facts}
   suffice.").

:- comment(doinclude, dynamic/1).

:- true decl dynamic(Predicates) : sequence_or_list(predname) + iso
        # "Defines each predicate in @var{Predicates} as a
          @concept{dynamic predicate}.  If a predicate is defined
          dynamic in a file, it must be defined dynamic in every file
          containing clauses for that predicate. The directive should
          precede all clauses of the affected predicates.  This
          directive is defined as a prefix operator in the compiler.".

:- pred asserta(+Clause) + iso
# "The current instance of @var{Clause} is interpreted as a clause and is
   added to the current program.  The predicate concerned must be dynamic.
   The new clause becomes the @em{first} clause for the predicate concerned.
   Any uninstantiated variables in @var{Clause} will be replaced by new
   private variables.".
%% along with copies of any subgoals blocked on these variables.

asserta(Clause) :-
        dynamic_clauses(Clause, Root, Ptr0, asserta/1),
	'$inserta'(Root, Ptr0).

:- pred asserta(+Clause,-Ref)
# "Like @tt{asserta/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

asserta(Clause, Ref) :-
        dynamic_clauses(Clause, Root, Ptr0, asserta/2),
	'$inserta'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).

:- pred assertz(+Clause) + iso
# "Like @tt{asserta/1}, except that the new clause becomes the @em{last}
   clause for the predicate concerned.".

assertz(Clause) :-
        dynamic_clauses(Clause, Root, Ptr0, assertz/1),
	'$insertz'(Root, Ptr0).

:- pred assertz(+Clause,-Ref)
# "Like @tt{assertz/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

assertz(Clause, Ref) :-
        dynamic_clauses(Clause, Root, Ptr0, assertz/2),
	'$insertz'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).

:- pred assert(+Clause)
# "Identical to @tt{assertz/1}. Included for compatibility.".

assert(Clause) :-
        dynamic_clauses(Clause, Root, Ptr0, assert/1),
	'$insertz'(Root, Ptr0).

:- pred assert(+Clause,-Ref)
# "Identical to @tt{assertz/2}. Included for compatibility.".

assert(Clause, Ref) :-
        dynamic_clauses(Clause, Root, Ptr0, assert/2),
	'$insertz'(Root, Ptr0),
	'$ptr_ref'(Ptr0, Ref).

dynamic_clauses(Clause, Root, Ptr0, Goal) :-
        term_to_meta(CL, Clause),
	(   canonical_clause(CL, Head, Body0),
	    wellformed_body(Body0, +, Body) ->
	    dynamic1(Head, Goal),
	    '$compile_term'([Head|Body], Ptr0), 
	    '$current_clauses'(Head, Root)
	;  
            throw(error(type_error(clause, CL), Goal-1))
	).

canonical_clause((H :- B), H, B) :- !,
	functor(H, F, _),
	atom(F).
canonical_clause(H, H, true) :-
	functor(H, F, _),
	atom(F).

:- comment(wellformed_body(BodyIn,Env,BodyOut),
   "@var{BodyIn} is a well-formed clause body. @var{BodyOut} is its
    counterpart with no single-variable meta-goals (i.e., with @tt{call(X)}
    for @tt{X}). @tt{Env} denotes if global cuts are admissible in
    @tt{BodyIn} (@tt{+} if they are, @tt{-} if they are not)."). 

wellformed_body(B, _, call(B)) :- var(B), !.
wellformed_body(!, Env, !) :- !, Env = + .
wellformed_body((A,B), Env, (A1,B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A->B), Env, (A1->B1)) :- !,
        wellformed_body(A, -, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A;B), Env, (A1;B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((\+ A), _, (\+ A1)) :- !,
        wellformed_body(A, -, A1).
wellformed_body(if(A,B,C), Env, if(A1,B1,C1)) :- !,
        wellformed_body(A, -, A1),
        wellformed_body(B, Env, B1),
        wellformed_body(C, Env, C1).
wellformed_body(L^A, Env, L^A1) :- !,
        wellformed_body(A, Env, A1).
wellformed_body(Goal, _, Goal) :-
	functor(Goal, F, _),
	atom(F).

dynamic(F/A, Mod) :-
        module_concat(Mod, F, MF),
        functor(Head, MF, A), !,
        asserta_fact(imports(Mod, Mod, F, A, Mod)), % defines/3 in not dynamic
	dynamic1(Head, dynamic/2).

% By now identical to dynamic
data(F/A, Mod) :-
        module_concat(Mod, F, MF),
        functor(Head, MF, A), !,
        asserta_fact(imports(Mod, Mod, F, A, Mod)), % defines/3 in not dynamic
	dynamic1(Head, data/2).

dynamic1(F, Goal) :-
	'$predicate_property'(F, _, Prop), !,
	(   Prop/\2 =:= 2 -> true		% dynamic, xref nondet.c
        ;   functor(F, N, A),
            throw(error(permision_error(modify, static_procedure, N/A), Goal))
	).
dynamic1(F, _) :-
	functor(F, Name, Ar),
	'$define_predicate'(Name/Ar, consult),
	'$set_property'(F, (dynamic)).		% xref indexing.c


:- pred retract(+Clause) + iso
# "The first clause in the program that matches @var{Clause} is erased.
   The predicate concerned must be dynamic. 

   The predicate @tt{retract/1}
   may be used in a non-determinate fashion, i.e., it will successively
   retract clauses matching the argument through backtracking. If reactivated
   by backtracking, invocations of the predicate whose clauses are being
   retracted will proceed unaffected by the retracts. This is also true
   for invocations of @tt{clause} for the same predicate. The space occupied
   by a retracted clause will be recovered when instances of the clause are
   no longer in use.".

retract(Clause) :-
        term_to_meta(CL, Clause),
	canonical_clause(CL, Head, Body), 
	dynamic1(Head, retract/1),
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, Body, Root, Ptr, no_block),
	'$erase'(Ptr),
        '$unlock_predicate'(Root).

:- pred retractall(+Head)
# "Erase all clauses whose head matches @var{Head}, where @var{Head} must
   be instantiated to an atom or a compound term.  The predicate concerned
   must be dynamic.  The predicate definition is retained.".

retractall(Head) :-
        term_to_meta(H, Head),
	nonvar(H),
        retractall_(H).

retractall_(Head) :-
	dynamic1(Head, retractall/1),
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, _, Root, Ptr, no_block),
	'$erase'(Ptr),
        '$unlock_predicate'(Root),
	fail.
retractall_(_).

:- pred abolish(+Spec) + iso
# "Erase all clauses of the predicate specified by the predicate spec
   @var{Spec}. The predicate definition itself is also erased (the
   predicate is deemed undefined after execution of the abolish). The
   predicates concerned must all be user defined.".

abolish(Spec) :-
        term_to_meta(F/A, Spec),
        functor(Head, F, A), !,
	abolish_data_of(Head),
	'$abolish'(Head).

:- multifile do_on_abolish/1.

:- comment(do_on_abolish(Head),"A hook predicate which will be called
	when the definition of the predicate of @var{Head} is abolished.").

abolish_data_of(Head) :-
        do_on_abolish(Head),
        fail.
abolish_data_of(_).

:- pred clause(+Head,?Body) +iso
# "The clause '@var{Head} @tt{:-} @var{Body}' exists in the current
   program. The predicate concerned must be dynamic.".

clause(HEAD, Body) :-
        term_to_meta(Head, HEAD),
	nonvar(Head),
	dynamic1(Head, clause/2),
	'$current_clauses'(Head, Root),
	'$current_instance'(Head, Body, Root, _, no_block),
        '$unlock_predicate'(Root).

:- comment(clause(Head,Body,Ref),"Like @tt{clause(Head,Body)}, plus the
   clause is uniquely identified by @var{Ref}.").

:- pred clause(+Head,?Body,?Ref)
# "@var{Head} must be instantiated to an atom or a compound term.".

:- pred clause(?Head,?Body,+Ref)
# "@var{Ref} must be instantiated to a valid identifier.".

clause(HEAD, Body, Ref) :-
	'$ptr_ref'(Ptr, Ref), !, 
	'$instance'(Head, Body, Ptr), 
	Head\==0,
        term_to_meta(Head, HEAD).
clause(HEAD, Body, Ref) :-
        term_to_meta(Head, HEAD),
	nonvar(Head), 
	dynamic1(Head, clause/3),
	'$current_clauses'(Head, Root), 
        '$current_instance'(Head, Body, Root, Ptr, no_block),
        '$unlock_predicate'(Root),
	'$ptr_ref'(Ptr, Ref).

:- pred current_predicate(?Spec) + iso
        # "A predicate in the current module is named @var{Spec}.".

:- pred current_predicate(?Spec,+Module)
        # "A predicate in @var{Module} is named @var{Spec}.".

current_predicate(F/A,M) :-
        module_concat(M,'',MPref),
        '$predicate_property'(P, _, _),
        functor(P, MF, A),
        atom_concat(MPref,F,MF).

% ----------------------------------------------------------------------

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*5+146,2000/05/19,21:01*42+'CEST'), "Implemented
   current_predicate/1 better.  Added current_predicate/2. (Daniel
   Cabeza Gras)").

:- comment(version(0*5+30,1998/06/30,14:23*37+'MET DST'), "Fixed bug in
   retract/1 -- unlocking before erasing (Daniel Cabeza Gras)").

:- comment(version(0*5+11,1998/05/23,19:30*30+'MET DST'), "Added
   dynamic/2 which use the new 'module' meta argument type (Daniel
   Cabeza Gras)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").


