:- module(tabling_rt_CHAT,
        [
            abolish_all_tables/0,      
	    tabled_call/1,
	    new_answer/0,
	    check/0,
 %% 	    test_answer/6,
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

:- true pred tabled_call(+Call) :: tablegoal 
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

:- true pred new_answer + foreign_low(new_answer_c)
 # "Adds the answer @var{Answer} to the generator
     identified by @var{Sid} and then fails.".

:- true pred check + foreign_low(check_c)
 # "Adds the answer @var{Answer} to the generator
     identified by @var{Sid} and then fails.".

 %% :- true pred test_answer(+Answer1, +Answer2, +Answer3, -Answer4, -Answer5, -Answer6) ::
 %%         answer * answer * answer * goal * goal * goal
 %%         + foreign_low(test_answer_c)
 %%  # "Adds the answer @var{Answer} to the generator
 %%      identified by @var{Sid} and then fails.".

:- doc(initial/0,"Initializes the trie module at the beginnig.").

:- true pred initial 
        + foreign_low(initial_c).

:- extra_compiler_opts(['-DTABLING']).


:- use_foreign_source(cont_calls).

:- initialization(initial).

:- doc(appendix, "An example of translation of a tabled predicate
   in order to execute it with SLG resolution is shown below:

 %% @begin{verbatim}
 %% :- use_package(library(tabling('SimulatedCHAT'))).
 %% :- table path/2.
 %% 
 %% path(X,Z) :- 
 %% 	edge(X,Y), 
 %% 	path(Y,Z).
 %% 
 %% path(X,Z) :-
 %%         edge(X,Z).
 %% @end{verbatim}
 %% 
 %%    translated to:
 %% 
 %% @begin{verbatim}
 %% path(X,Y) :- 
 %% 	tabled_call(path(X,Y), Sid, 'path_PADL:path0', 'true', Pred),
 %% 	(
 %% 	    '$meta_call'(Pred) ->
 %% 	    consume_answer(path(X,Y),Sid)
 %% 	;
 %% 	    resume_ccalls(Sid,CCall,0,0,0),
 %% 	    '$meta_call'(CCall),
 %% 	    consume_answer(path(X,Y),Sid)
 %% 	).
 %% 
 %% path0(path(X,Y),Sid) :- 
 %% 	edge(X,Z),
 %% 	tabled_call(path(Z,Y), NewSid, 'path_PADL:path0', 'path_PADL:path1', Pred),
 %% 	(
 %% 	    '$meta_call'(Pred) ->
 %% 	    true
 %% 	;
 %% 	    resume_ccalls(NewSid,CCall,0,0,0),
 %% 	    '$meta_call'(CCall)	    
 %% 	),
 %% 	new_ccall(Sid,NewSid,[X],F,0),
 %% 	'$meta_call'(F).
 %% 
 %% path0(path(X,Y),Sid) :-
 %% 	edge(X,Y),
 %% 	new_answer(path(X,Y),Sid).
 %% 
 %% path1(path(_,Y),Sid,[Z]) :-
 %% 	new_answer(path(Z,Y),Sid).
 %% @end{verbatim} ").

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
