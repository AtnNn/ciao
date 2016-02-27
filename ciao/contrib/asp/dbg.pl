:- module(dbg, [dmsg/2, dcall/2]).

:- use_module(misc).

% debug_flag used for debug prining messages.
:- data debug_flag/1, pred_flag/1.

debug_flag(1).
debug_flag(2).
debug_flag(3).
debug_flag(4).
debug_flag(5).

pred_flag(1).
pred_flag(2).
pred_flag(3).
pred_flag(4).
pred_flag(5).

% Predicates used for debugging:

% dmsg stands for debug message.
% DLevel stands for debug level.
dmsg(DLevel,Msg) :-
	list(Msg),
	debug_flag(DLevel),
	disp_list(Msg),!.
dmsg(DLevel,Msg) :-
	debug_flag(DLevel),
	display(Msg),!.
dmsg(_,_).

% dcall stands for debug call.
dcall(DLevel,Pred) :-
	pred_flag(DLevel),
	call(Pred),!.
dcall(_,_).

