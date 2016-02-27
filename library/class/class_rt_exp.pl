%%------------------------------------------------------------------------
%%
%% O'CIAO: Object Oriented Programming in CIAO/Prolog
%%
%% RUN-TIME EXPANSION FOR CLASS CODE
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% NOTICE: this code is included in all classes when compiling.
%%         Used for run-time expand meta-programming structures inside
%%         classes.
%%
%%------------------------------------------------------------------------
%% WARNING:
%%   Those predicates are used by O'CIAO class expansion.
%%   Dont call directly !!!!
%%------------------------------------------------------------------------

rt_exp(Inst,Goal,Exp) :-
