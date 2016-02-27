:- module(memo_rt,
        [
            abolish_all_tables/0,                                                                   
	    memo_call/3,
	    new_answer/2,
	    consume_answer/3, 
	    set_complete/1, 
	    memogoal/1,
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
 # "It removes all meoizations presently in
     the system and frees all the memory held by Ciao for these
     structures.".

:- true pred memo_call(+Call,?Sid,-Pred) ::
	memogoal * id * goal
	+ foreign_low(memo_call_c)
 # "Instruments calls to the memo predicate
     @var{Call}. It checks the call to be marked as generator or
     consumer using a trie structure. Generators resolve against
     original program clauses of the tabled predicate and consumers
     reads answers from the table memory and suspends when there is
     not any unconsumed answers. @var{Sid} is an identifier for the table
     entry of @var{Call}.".

:- true pred new_answer(+Answer, +Sid) ::
        answer * id
        + foreign_low(new_answer_c)
 # "Adds the answer @var{Answer} to the generator
     identified by @var{Sid} and then fails.".

:- true pred set_complete(+Sid) ::
        id
        + foreign_low(set_complete_c)
 # "Sets the table entry @var{Sid} as complete.".

:- true pred consume_answer(-Answer, +Sid, ?D) ::
       answer * id * dummy
        + foreign_low(consume_answer_c)
 # "Returns each of the found answers of
     the generator identified by @var{Sid} unifying them with
     @var{Answer} by backtracking.".

:- doc(initial/0, "Initializes the trie module at the beginnig.").

:- extra_compiler_opts(['-DTABLING']).

:- true pred initial 
        + foreign_low(initial_c).

:- use_foreign_source(memo).

:- initialization(initial).

:- regtype memogoal(T) # "@var{T} is a memo goal.".
memogoal(_).

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
