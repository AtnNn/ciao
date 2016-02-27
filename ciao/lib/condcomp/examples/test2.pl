:- module(_, [main/0], [condcomp]).

% Try to comment or uncomment the following lines:
:- define(use_write).
:- define(use_quote).

:- if(defined(use_write)).
    :- use_module(library(write)).
    :- if(defined(use_quote)).
        pr(X) :- writeq(using_write(X)), nl.
    :- else.
        pr(X) :- write(using_write(X)), nl.
    :- endif.
:- else.
    :- if(defined(use_quote)).
        pr(X) :- displayq(using_display(X)), nl.
    :- else.
        pr(X) :- display(using_display(X)), nl.
    :- endif.
:- endif.

main :-
	pr('hello world').
