:- module(make_tr, [defdep/3,concat_bodies/3], ['make/make_ops']).

%% Temporary (for debugging)
:- use_module(library(write)).

%% debug_tr(off).
debug_tr(on).

defdep( ( T <= S :: F :- Body ), Clauses, _Mod) :- 
	!,
	Clauses = [ (  do_dependency(T,S,F) :- Body   ), 
	            (  dependency_exists(T,S) :- true ) ], 
	debug_transformation(Clauses).

%% defdep(   (  T <= S :- Body               ), Clauses, _Mod) :- 
%% 	!,
%% 	Clauses = [ (  do_dependency(T,S,_) :- Body    ), 
%% 	            (  dependency_exists(T,S) :- true  ) ], 
%% 	debug_transformation(Clauses).

defdep( ( Target <- Deps :- Body ), Clauses, _Mod) :- 
	!,
	Clauses = [ (  do_target(Target)        :- Body  ), 
	            (  target_exists(Target)    :- true  ), 
	            (  target_deps(Target,Deps) :- true  )	], 
	debug_transformation(Clauses).

defdep( ( Target <- :- Body ), Clauses, _Mod) :-
	!,
	Clauses = [ (  do_target(Target) :- Body      ), 
	            (  target_exists(Target) :- true  ), 
	            (  target_deps(Target,[]) :- true )	],
	debug_transformation(Clauses).

concat_bodies((G, Gs), B, (G, NB)) :- !,
        concat_bodies(Gs, B, NB).
concat_bodies(G, B, (G, B)).

% --- debugging ---

debug_transformation(_Clauses) :-
	debug_tr(off),
	!.
debug_transformation(Clauses) :-
	debug_tr(on),
	!,
	prolog_flag(write_strings,Old,on),
	write('*** New set of clauses: '), nl,
	write_clauses(Clauses),
	nl,
	prolog_flag(write_strings,_,Old).

write_clauses([]) :-
	!.
write_clauses([Clause|Clauses]) :-
	!,
	write_clause(Clause),
	write_clauses(Clauses).
write_clauses(Clause) :- 
	write('*** Strange format found ***:'), nl,
	write_clause(Clause).

write_clause(Clause) :-
	write(Clause),write('.'),nl.
