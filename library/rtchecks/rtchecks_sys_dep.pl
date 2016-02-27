%% system_dependent_compatible(Prop):-
%% 	fail.
% Next definition requires the system to be "quick rejecting". 
% I. e., those states which are inconsistent should always be rejected.
system_dependent_compatible(Prop):-
	\+(\+(call(Prop))).

% This definition works in any system
system_dependent_incompatible(Prop):-
	\+(call(Prop)).

%% system_dependent_entailed(Prop):-
%% 	fail.
% This is correct everywhere, but only useful if Prop does not contain dvars
system_dependent_entailed(Prop):-
	copy_term(Prop,NProp),
	call(NProp),
	instance(Prop,NProp).

% This is correct and useful in any system
system_dependent_disentailed(Prop):-
	system_dependent_incompatible(Prop),!.
% This is only correct for complete solvers (such as herbrand)
system_dependent_disentailed(Prop):-
	copy_term(Prop,NProp),
	call(NProp),
	\+(instance(Prop,NProp)),
	instance(NProp,Prop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
show_prop(Prop):-
	write('ERROR: false program point assertion'),	nl,
	write(Prop),nl.
show_prog_point(PP):-
	write('ERROR: false program point assertion'),	nl,
	write('at '),
	write(PP),nl.
show_prop_and_prog_point(Prop,PP):-
	write('ERROR: false program point assertion'),	nl,
	write(Prop),nl,
	write('at '),
	write(PP),nl.
