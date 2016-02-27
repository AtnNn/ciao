:- module(term_to_string, [term_to_string/2], []).

:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(strings)).

term_to_string(T,S) :-
      mktemp('/tmp/t2sXXXXXX',TMP),
      open_output(TMP,Out),
      display(T),
      close_output(Out),
      open_input(TMP, In),
      get_line(S),
      close_input(In),
      delete_file(TMP).
