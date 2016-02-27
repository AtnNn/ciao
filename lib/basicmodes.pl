
:- op(500,  fx,(?)).
:- op(500,  fx,(@)).

%% "ISO-like" modes
:- modedef '+'(A) : nonvar(A).
:- modedef '-'(A) : var(A).
:- modedef '?'(_).
:- modedef '@'(A) + not_further_inst(A).

%% Useful input-output modes
:- modedef in(A)  : ground(A) => ground(A).
:- modedef out(A) : var(A)    => ground(A).
:- modedef go(A)              => ground(A).

%% Parametric versions of above
:- modedef '+'(A,X) :: X(A) : nonvar(A).
:- modedef '-'(A,X) :: X(A) : var(A).
:- modedef '?'(A,X) :: X(A).
:- modedef '@'(A,X) :: X(A) + not_further_inst(A).
:- modedef in(A,X)  :: X(A) : ground(A) => ground(A).
:- modedef out(A,X) :: X(A) : var(A)    => ground(A).
:- modedef go(A,X)  :: X(A)             => ground(A).

%% :- comment(version_maintenance,off)).

