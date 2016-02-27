
:- module(c_itf_props,[ moddesc/1, filename/1 ],[ assertions, regtypes ]).

:- comment(title,"Some types and properties related to c_itf").

:- comment(author,"F. Bueno").

% ---------------------------------------------------------------------------
:- prop moddesc(X) + regtype # "@var{X} is a module descriptor.".
% ---------------------------------------------------------------------------

moddesc(X)       :- atom(X).
moddesc(user(X)) :- atom(X).

% ---------------------------------------------------------------------------
:- regtype filename(X) 
   # "@var{X} is an atom describing the name of a file.".
% ---------------------------------------------------------------------------

filename(X) :- 
	atm(X).


% ---------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version/')).

:- comment(version(1*7+89,2001/04/19,13:15*47+'CEST'), "Added
   assertions(c_itf_props) as a kludge.  (Francisco Bueno Carrillo)").

% ---------------------------------------------------------------------------
