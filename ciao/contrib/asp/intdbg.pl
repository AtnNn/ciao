:- module(intdbg, [buildDbg/1]).

buildDbg(S) :-

% debug_flag used for debug prining messages.
write(S,':- data debug_flag/1, pred_flag/1.\n'),

% Predicates used for debugging:

% dmsg stands for debug message.
% dLevel stands for debug level.
write(S,'dmsg(dLevel,Msg) :-\n'),
write(S,'    list(Msg),\n'),
write(S,'    debug_flag(dLevel),\n'),
write(S,'    disp_list(Msg),!.\n'),
write(S,'dmsg(dLevel,Msg) :-\n'),
write(S,'    debug_flag(dLevel),\n'),
write(S,'    display(Msg),!.\n'),
write(S,'dmsg(_,_).\n\n'),

% dcall stands for debug call.
write(S,'dcall(dLevel,Pred) :-\n'),
write(S,'    pred_flag(dLevel),\n'),
write(S,'    call(Pred),!.\n'),
write(S,'dcall(_,_).\n\n').


