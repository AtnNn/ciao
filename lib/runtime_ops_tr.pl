:- module(runtime_ops_tr, [runtime_op/2],[assertions]).

runtime_op((:- Op), RuntimeOp) :-
        Op = op(_,_,_),
        RuntimeOp = [(:- Op), (:- initialization(Op))].

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
