:- module(counters, [setcounter/2, getcounter/2, inccounter/2],[assertions]).

:- data counter/2.

setcounter(Name, Val) :-
        retractall_fact(counter(Name, _)),
        asserta_fact(counter(Name, Val)).

getcounter(Name, Val) :-
        current_fact(counter(Name, Val)).

inccounter(Name, Val) :-
        retract_fact(counter(Name, Val)),
        Val1 is Val+1,
        asserta_fact(counter(Name, Val1)).

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

