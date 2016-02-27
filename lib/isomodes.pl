
:- op(200, fy, [(?),(@)]).

%% Basic ISO-modes
:- modedef '+'(A) : nonvar(A).
:- modedef '@'(A) + not_further_inst(A).
:- modedef '-'(A) : var(A).
:- modedef '?'(_).
:- modedef '*'(_).

%% Parametric versions of above
:- modedef +(A,X) :  X(A).
:- modedef @(A,X) :  X(A) => X(A) + not_further_inst(A).
:- modedef -(A,X) :  var(A) => X(A).
:- modedef ?(A,X) :: X(A) => X(A).
:- modedef *(A,X) :: X(A).

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

