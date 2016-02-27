:- module(inherit2_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Test for inheritance (measures)").
% TODO: Rewrite this test. This program does many things:
%  - defines a new 'primitive type' (using inheritance)
%  - add +, /, > operations to the primitive type, as hooks 
%  - but it is not a constraint solver! so it is not very generic indeed

:- use_module(library(arithpreds)).
:- use_module(engine(io_basic)).

:- export(main/0).
main :-
	measures.main.

% Distances
:- class distance {
    :- export(s/1).
    :- attr s.

    :- virtual name/1.
    :- virtual norm/1.
    :- virtual cons/2.

    :- export(cons__/1).
    cons__(X) :- ~s = X.

    % TODO: extend an interface for those operations
    :- export('$to_str'/1).
    '$to_str' := ~s + " " + ~name.

    :- export('$add'/2).
    '$add'(Y) := ~cons(~norm.s + ~Y.norm.s).
    :- export('$div'/2).
    '$div'(Y) := ~norm.s / ~Y.norm.s.
    :- export('$gt'/1).
    '$gt'(Y) :- ~norm.s > ~Y.norm.s.
}.
% Distances in the I.S. units
% TODO: FIX!!!!!!!!!!!!!!!!!!!!!!
% TODO: bug! no cons__! no initialization! look at inits from parents...
:- op(50, yf, [m, dm, cm, mm]).
:- fun_eval(m/1).
:- class (m) { 
    :- extends distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    :- export(norm/1).
    norm := ~self. 
    :- export(cons/2).
    cons(X) := X m. 
    :- export(name/1).
    name := "meters".
}.
% Distances that normalize to 'm'
:- class m_distance {
    :- extends distance.
    :- virtual f/1.
    norm := (~self.s * ~f) m.
    :- export(cons/2).
    cons(X) := X m. % TODO: strange...
}.
% Definition of other standard distances
:- fun_eval(dm/1).
:- class (dm) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 0.1.
    :- export(name/1).
    name := "decimeters".
}.
:- fun_eval(cm/1).
:- class (cm) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 0.01.
    :- export(name/1).
    name := "centimeters".
}.
:- fun_eval(mm/1).
:- class (mm) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 0.001.
    :- export(name/1).
    name := "millimeters".
}.
% Distances in the Imperial units
:- op(50, yf, [inches, feet, yards, furlongs, miles]).
:- fun_eval(inches/1).
:- class (inches) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 25.4 mm / 1 m.
    :- export(name/1).
    name := "inches".
}.
:- fun_eval(feet/1).
:- class (feet) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 12 inches / 1 m.
    :- export(name/1).
    name := "feet".
}.
:- fun_eval(yards/1).
:- class (yards) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 3 feet / 1 m.
    :- export(name/1).
    name := "yards".
}.
:- fun_eval(furlongs/1).
:- class (furlongs) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 220 yards / 1 m.
    :- export(name/1).
    name := "furlongs".
}.
:- fun_eval(miles/1).
:- class (miles) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit from 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 8 furlongs / 1 m.
    :- export(name/1).
    name := "miles".
}.

% TODO: with clp it is possible to ask things like: 10 cm = X m.
:- module measures {
    main :-
        display("Creating a distance..."), nl,
        M = 2 dm,
        display("Created, its value is:"), nl,
        display(M), nl,
        display("The normalized value is:"), nl,
        display(~M.norm), nl,
        display("Some distances:"), nl,
        display(1 m), nl,
        display(2 dm), nl,
        display(3 cm), nl,
        display(4 mm), nl,
        display("Sum of distance is:"), nl,
        display(1 m + 2 dm + 3 cm + 4 mm), nl.
    
%test :-
%    1.234 m = 1 m + 2 dm + 3 cm + 4 mm,
%    10 miles + 1 furlongs > 10 m.
}.
