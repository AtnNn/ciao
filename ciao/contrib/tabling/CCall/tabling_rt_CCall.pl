:- module(tabling_rt_CCall,
        [
            abolish_all_tables/0,      
	    tabled_call/5,
	    resume_ccalls/5,
	    new_ccall/6,
	    new_answer/2,
	    consume_answer/2, 
	    tablegoal/1,
	    goal/1,
	    id/1,
	    pred_name/1,
	    cont/1,
	    dummy/1,
	    bindings/1,
	    answer/1
	], 
	[assertions,hiord,regtypes,foreign_interface]).

:- true pred abolish_all_tables    
        + foreign_low(clean_tables_c)
 # "It removes all tables presently in
     the system and frees all the memory held by Ciao for these
     structures. Predicates that have been declared tabled remain so,
     but information in their table is deleted. abolish all tables/0
     works directly on the memory structures allocated for table
     space. This makes it very fast for abolishing a large amount of
     tables.".

:- true pred tabled_call(+Call,?Sid,+First,+CCall,-Pred) ::
	tablegoal * id * pred_name * pred_name * goal
	+ foreign_low(tabled_call_c)
 # "Instruments calls to the tabled predicate
     @var{Call}. It checks the call to be marked as generator or
     consumer using a trie structure. Generators resolve against
     original program clauses of the tabled predicate and consumers
     reads answers from the table memory and suspends when there is
     not any unconsumed answers. @var{Sid} is an identifier for the table
     entry of @var{Call} and @var{CCall} is associated as its
     continuation. @var{Pred} is a @var{First} clause apropiated
     instanciated to be called if we are in a generator, or a free var
     if we are in a consumer.".

:- true pred resume_ccalls(+Sid, -Call, ?C, ?D, ?E)  ::
	id * cont * dummy * dummy * dummy
        + foreign_low(resume_ccalls_c)
 # "Resumes all the consumers of the generator
     identified by @var{Sid} consuming all their pending
     answers. @var{Call} is unified with each of the continuations by
     backtracking and then, checks for completion. This primitive
     should be used after the execution of a generator. The three last
     arguments should not be considered, they are a trick to add extra
     slots in the choice points without changing their general
     structure.".

:- true pred new_ccall(+Sid, +NewSid, +Bindings, -F, ?E, ?G) ::
        id * id * bindings * cont * dummy * dummy
        + foreign_low(new_ccall_c)
 # "Adds a new continuation using the list of
     bindings @var{Bindings} to recover the environment of a consumer. The
     continuation is added to the generator identified by
     @var{NewSid}, and it depends on the generator identified by
     @var{Sid}. @var{F} is unified with the generated continuation and
     it consumes all the pendings answers by backtracking. The last
     argument should not be considered, it is a trick to add an extra
     slot in the choice points without changing their general
     structure.".

:- true pred new_answer(+Answer, +Sid) ::
        answer * id
        + foreign_low(new_answer_c)
 # "Adds the answer @var{Answer} to the generator
     identified by @var{Sid} and then fails.".

:- true pred consume_answer(-Answer, +Sid) ::
       answer * id 
        + foreign_low(consume_answer_c)
 # "Returns each of the found answers of
     the generator identified by @var{Sid} unifying them with
     @var{Answer} by backtracking.".

:- doc(initial/0,"Initializes the trie module at the beginnig.").

:- true pred initial 
        + foreign_low(initial_c).

:- extra_compiler_opts(['-DTABLING']).


:- use_foreign_source(cont_calls).

:- initialization(initial).

:- doc(appendix, "An example of translation of a tabled predicate
   in order to execute it with SLG resolution is shown below:

@begin{verbatim}
:- use_package(library(tabling('CCall'))).
:- table path/2.

path(X,Z) :- 
	edge(X,Y), 
	path(Y,Z).

path(X,Z) :-
        edge(X,Z).
@end{verbatim}

   translated to:

@begin{verbatim}
path(X,Y) :- 
	tabled_call(path(X,Y), Sid, 'path_PADL:path0', 'true', Pred),
	(
	    '$meta_call'(Pred) ->
	    consume_answer(path(X,Y),Sid)
	;
	    resume_ccalls(Sid,CCall,0,0,0),
	    '$meta_call'(CCall),
	    consume_answer(path(X,Y),Sid)
	).

path0(path(X,Y),Sid) :- 
	edge(X,Z),
	tabled_call(path(Z,Y), NewSid, 'path_PADL:path0', 'path_PADL:path1', Pred),
	(
	    '$meta_call'(Pred) ->
	    true
	;
	    resume_ccalls(NewSid,CCall,0,0,0),
	    '$meta_call'(CCall)	    
	),
	new_ccall(Sid,NewSid,[X],F,0),
	'$meta_call'(F).

path0(path(X,Y),Sid) :-
	edge(X,Y),
	new_answer(path(X,Y),Sid).

path1(path(_,Y),Sid,[Z]) :-
	new_answer(path(Z,Y),Sid).
@end{verbatim} ").

:- regtype tablegoal(T) # "@var{T} is a tabled goal.".
tablegoal(_).

:- regtype goal(T) # "@var{T} is a Prolog goal.".
goal(_).

:- regtype id(T) # "@var{T} is an integer.".
id(_).

:- regtype pred_name(T) # "@var{T} is an atom.".
pred_name(_).

:- regtype cont(T) # "@var{T} is a Prolog goal.".
cont(_).

:- regtype dummy(T) # "@var{T} is the zero integer.".
dummy(0).

:- regtype bindings(T) # "@var{T} is a list.".
bindings(T) :- list(T).

:- regtype answer(T) # "@var{T} is
   a Prolog term.".
answer(_).
