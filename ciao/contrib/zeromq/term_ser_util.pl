:- module(term_ser_util, 
	[
	    string/1,
	    string_list/2
	]).

% -- Auxiliary -----------------------------------------------------

string(_):- fail.

string_list(X, X).


