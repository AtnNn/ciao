:- module(terms, [copy_args/3, arg/2, atom_concat/2],[assertions]).


copy_args(0, _, _) :- !.
copy_args(I, F1, F2) :-
	arg(I, F1, X),
	arg(I, F2, X),
	I1 is I-1,
	copy_args(I1, F1, F2).

% Gives on backtracking the arguments of a term
arg(T, A) :-
        functor(T, _, N),
        args(1, N, T, A).

args(X, _, T, A) :-
        arg(X, T, A).
args(X, M, T, A) :-
        X < M,
        X1 is X+1,
        args(X1, M, T, A).

%-------------------------------------------------------------------------
:- comment(atom_concat(Atms,Atm),"@var{Atm} is the atom resulting from
	concatenating all atoms in the list @var{Atms} in the order
	in which they appear.").

atom_concat([],'').
atom_concat([A1|Atoms],Atom):-
        atoms_concat(Atoms, A1, Atom).

atoms_concat([], Atom, Atom).
atoms_concat([A1|Atoms], A2, Atom) :-
        atom_concat(A2, A1, A3),
        atoms_concat(Atoms, A3, Atom).


% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*3+124,1999/11/27,04:04*05+'MET'),
   "@pred{atom_concat/2} was moved here.  (Manuel Hermenegildo)").

:- comment(version(0*5+2,1998/3/31), "Eliminated superflous definition
   for atom_concat/3.  (Manuel Hermenegildo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

% ----------------------------------------------------------------------------


