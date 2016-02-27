:- module(_, _, [assertions, isomodes]).
%%**********************************************************************
%% GOAL SCHEDULING
%%**********************************************************************


:- pred goal_scheduling(-atm).

:- doc(goal_scheduling(GS), "Determines the goal scheduling to
   follow by the different agents in the system.").

goal_scheduling(fifo).
% goal_scheduling(lifo).

