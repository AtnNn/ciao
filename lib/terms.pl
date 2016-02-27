:- module(terms,[ copy_args/3, arg/2, atom_concat/2 ],[assertions]).

:- comment(title,"Term manipulation utilities").

%-------------------------------------------------------------------------
:- pred copy_args(N,Term,Copy) : nnegint(N)
      # "@var{Term} and @var{Copy} have the same first @var{N}
         arguments.".

copy_args(0, _, _) :- !.
copy_args(I, F1, F2) :-
	arg(I, F1, X),
	arg(I, F2, X),
	I1 is I-1,
	copy_args(I1, F1, F2).

%-------------------------------------------------------------------------
:- pred arg(Term,Arg) 
      # "@var{Arg} is an argument of @var{Term}. Gives each of the
         arguments on backtracking.".

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

:- comment(version(1*7+141,2001/11/12,17:47*36+'CET'), "Added doc for
   all preds.  (Francisco Bueno Carrillo)").

:- comment(version(1*3+124,1999/11/27,04:04*05+'MET'),
   "@pred{atom_concat/2} was moved here.  (Manuel Hermenegildo)").

:- comment(version(0*5+2,1998/3/31), "Eliminated superflous definition
   for atom_concat/3.  (Manuel Hermenegildo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

% ----------------------------------------------------------------------------
