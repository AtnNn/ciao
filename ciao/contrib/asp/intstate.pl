:- module(intstate,[buildAspState/1]).

buildAspState(S) :- 

write(S,':- data curr_state/1, state/1, stateChanged/0.\n'),
write(S,'curr_state(0).'), nl(S),
write(S,'%------------------------------------------------'), nl(S),
write(S,'% curr_state handler...'), nl(S),
write(S,'%-----------------------'), nl(S),
write(S,'inc_state :- curr_state(State),'), nl(S),
write(S,'	state_exists(State,State1),'), nl(S),
write(S,'       retractall(state(_)),\n'),
write(S,'	deleteGroundASP,\n'),
write(S,'	push_state(State1).\n\n'),

write(S,'dec_state :- curr_state(N), N>0, !,\n'),
%write(S,'    display(''cuurent_state=''),display(N),nl,\n'),
write(S,'	retract_fact(curr_state(N)),\n'),
write(S,'	deleteGroundASP,\n'),
write(S,'	remState(N),\n'),
write(S,'	retractall_fact(state(_)).\n'),
write(S,'dec_state :- curr_state(0), !, inc_state.\n'),
write(S,'dec_state :- assertz_fact(state(0)).\n\n'),

write(S,'getCState(State) :- curr_state(State).\n\n'),

write(S,''), nl(S),
write(S,'get_state(State) :- curr_state(State),!.\n'),
write(S,'get_state(State) :- var(State), !,\n'),
write(S,'	state_exists(0,State),\n'),
write(S,'	set_curr_state(State),curr_state(State).\n\n'),

write(S,'push_state(State) :- curr_state(State),!.\n'),
write(S,'push_state(State) :- asserta_fact(curr_state(State)).\n\n'),

write(S,'pop_state :- curr_state(State),\n'),
write(S,'       remState(State),\n'),
write(S,'       State>0, !,\n'),
write(S,'	retract_fact(curr_state(State)).\n\n'),

write(S,'state_exists(State,State1) :-'), nl(S),
write(S,'       aspModelExist(State),'), nl(S),
%write(S,'	aspModel(State,_,_),'), nl(S),
write(S,'	State2 is State+1, !,'), nl(S),
write(S,'	state_exists(State2,State1).'), nl(S),
write(S,'state_exists(State,State).'), nl(S), nl(S),

write(S,'set_curr_state(_) :- retractall_fact(curr_state(_)), fail.'), nl(S),
write(S,'set_curr_state(N) :- assertz_fact(curr_state(N)).'), nl(S),
write(S,''), nl(S),

write(S,'rem_curr_state :- retractall_fact(curr_state(_)), !.'), nl(S),
write(S,'rem_curr_state.'), nl(S),
write(S,''), nl(S),
write(S,'%------------------------------------------------\n'), nl(S),

write(S,'incParentState :-\n'),
write(S,'	parent(L),\n'),
%write(S,'display(''incParentState:parents L=''), display(L), nl,\n'),
write(S,'	stateChanged,!,\n'),
%write(S,'display(''incEachParentState''), nl,\n'),
write(S,'	incEachParentState(L),\n'),
%write(S,'display(''retractall_Fact(stateChanged)''), nl,\n'),
write(S,'	retractall_fact(stateChanged).\n'),
write(S,'incParentState.\n\n'),

write(S,'incEachParentState([]).\n'),
write(S,'incEachParentState([H|T]) :-\n'),
%write(S,'	use_module(H),\n'),
write(S,'	atom_concat(H, '':'', H1),\n'),
write(S,'	atom_concat(H1,''setStateChanged'',ChangeState),\n'),
write(S,'	atom_concat(H1, ''incState'', IncState),\n'),
%write(S,'display(''ChangeState=''),display(ChangeState),nl,\n'),
%write(S,'display(''IncState=''),display(IncState),nl,\n'),
write(S,'	''$meta_call''(ChangeState),\n'),
write(S,'	''$meta_call''(IncState),\n'),
%write(S,'display(''incEachParentState for T=''), display(T), nl,\n'),
write(S,'	incEachParentState(T).\n\n'),

write(S,'decParentState :-\n'),
write(S,'	parent(L),\n'),
%write(S,'display(''decParentState:parents L=''), display(L), nl,\n'),
write(S,'	stateChanged,!,\n'),
%write(S,'display(''decEachParentState''), nl,\n'),
write(S,'	decEachParentState(L),\n'),
%write(S,'display(''retractall_Fact(stateChanged)''), nl,\n'),
write(S,'	retractall_fact(stateChanged).\n'),
write(S,'decParentState.\n\n'),

write(S,'decEachParentState([]).\n'),
write(S,'decEachParentState([H|T]) :-\n'),
%write(S,'	use_module(H),\n'),
write(S,'	atom_concat(H, '':'', H1),\n'),
write(S,'	atom_concat(H1,''setStateChanged'',ChangeState),\n'),
write(S,'	atom_concat(H1, ''dec_state'', DecState),\n'),
%write(S,'display(''ChangeState=''),display(ChangeState),nl,\n'),
%write(S,'display(''DecState=''),display(DecState),nl,\n'),
write(S,'	''$meta_call''(ChangeState),\n'),
write(S,'	''$meta_call''(DecState),\n'),
%write(S,'display(''decEachParentState for T=''), display(T), nl,\n'),
write(S,'	decEachParentState(T).\n\n'),

write(S,'incState :-\n'),
write(S,'	stateChanged,!,\n'),
write(S,'	inc_state, incParentState,\n'),
write(S,'	self(ModuleName),\n'),
%write(S,'     display(''resetStateChanged''), nl,\n'),
write(S,'	module_concat(ModuleName,resetStateChanged,G),\n'),
%write(S,'	atoms_concat([ModuleName,'':'',resetStateChanged], G),\n'),
write(S,'	''$meta_call''(G).\n'),
write(S,'incState.\n\n'),

write(S,'decState :-\n'),
%write(S,'    display(''decState''), nl, \n'),
write(S,'	stateChanged,!,\n'),
write(S,'	dec_state, decParentState,\n'),
write(S,'	self(ModuleName),\n'),
write(S,'	module_concat(ModuleName,resetStateChanged,G),\n'),
%write(S,'	atoms_concat([ModuleName,'':'',resetStateChanged], G),\n'),
write(S,'	''$meta_call''(G).\n'),
write(S,'decState.\n\n'),

write(S,':- export(setStateChanged/0).\n'),
write(S,':- export(resetStateChanged/0).\n'),
write(S,'setStateChanged :- stateChanged,!.\n'),
write(S,'setStateChanged :- assertz_fact(stateChanged).\n\n'),

write(S,'resetStateChanged :- stateChanged,!,\n'),
write(S,'	retractall_fact(stateChanged).\n'),
write(S,'resetStateChanged.\n\n').

