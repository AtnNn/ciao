:- module(persdbtr_sql,[sql_persistent_tr/2, sql_goal_tr/2,
	dbId/2],[assertions]).

%:- reexport(library('persdb_sql/db_client',
%	            [socketname/1,dbname/1,user/1,passwd/1])).
%:- reexport(library('persdb_sql/pl2sql',
%	            [projterm/1,querybody/1])).

:- use_module(library(dynamic)).

:- data dbId/2.

sql_persistent_tr( (:- sql_persistent(PrologDef,SQLDef,DBId)), ClauseList) :-
 	functor(PrologDef,PrologName,Arity),
 	functor(Consequence,PrologName,Arity),
	ClauseList =  [ '$is_sql_persistent'(PrologDef,SQLDef,DBId),
 			(Consequence :- db_call_db_atomic_goal(DBId,Consequence)) ],
	assertz_fact(dbId(DBId,Consequence)).

%jcf	
% If a persistent predicate is also defined as a normal predicate inside the same module, 
% queries to that predicate should provide wrong results (first database rows, then 
% prolog clauses), but it doesn't. THIS MUST BE CHECKED!

sql_goal_tr( Goal, db_call_db_atomic_goal(DBId,Goal)) :-
	current_fact(dbId(DBId,Goal)).
%jcf


% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+42,2002/12/13,17:55*42+'CET'),
   "sql_persistent_tr/2 modified to call directly
   db_call_db_atomic_goal/2 instead of persistent predicates in all
   the clauses of the module (Jesus Correas Fernandez)").

:- comment(version(0*7+17,1998/10/01,10:29*56+'MET DST'), "Name/Arity
   compatibility with the file based persistent predicates completed (Jose
   Manuel Gomez Perez)").

:- comment(version(0*5+34,1998/06/30,19:23*36+'MET DST'), "Converted
   to an expansion.  (Manuel Hermenegildo)").

:- comment(version(0*5+23,1998/06/24,09:27*14+'MET DST'),
   "Incorporated into ciao library directory.  (Manuel
   Hermenegildo)").

:- comment(version(0*1+2,1998/06/16,13:28*47+'MET DST'), "Started autodoc.
   (Ignacio Caballero Blanco)").

% ----------------------------------------------------------------------------






:- comment(version_maintenance,dir('../../version')).

