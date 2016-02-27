:- module(intclean, [buildCleanup/1]).


buildCleanup(S) :-
write(S,''), nl(S),
write(S,'% reset_asp : deletes all stored models.'), nl(S),
write(S,'%             initialize: curr_state(0).'), nl(S),
write(S,'reset_asp :- set_curr_state(0),'), nl(S),
write(S,'    remove_all_classes,\n'),
write(S,'	deleteGroundASP,\n'),
%write(S,'	retractall_fact(aspState(_)),\n'),
%write(S,'	retractall_fact(exe_code(_)),\n'),
write(S,'	retractall_fact(endModels(_)).\n'),
%write(S,'	retractall_fact(curr_data(_)).\n'),

write(S,'remove_all_classes :- \n'),
write(S,'	retractall_fact(aspModel(_,_,_)),\n'),
write(S,'	retractall_fact(wfmState(_,_)),\n'),
write(S,'	retractall_fact(stateSkep(_,_)),\n'),
write(S,'	retractall_fact(stateAtom(_,_)).\n\n'),

write(S,'remState(State) :- \n'),
write(S,'	remStateClasses(State),\n'),
write(S,'	retractall_fact(endModels(State)).\n\n'),

write(S,'remStateClasses(St) :- \n'),
write(S,'	retractall_fact(aspModel(St,_,_)),\n'),
write(S,'	retractall_fact(wfmState(St,_)),\n'),
write(S,'	retractall_fact(stateSkep(St,_)),\n'),
write(S,'	retractall_fact(stateAtom(St,_)).\n\n').




