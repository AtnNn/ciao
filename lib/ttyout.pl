:- module(ttyout, [
        ttyget/1, ttyget1/1, ttynl/0, ttyput/1, ttyskip/1, ttytab/1,
        ttyflush/0, ttydisplay/1, ttydisplayq/1, ttyskipeol/0,
        ttydisplay_string/1
        ],[assertions]).


ttyput(X) :- put_code(user, X).

ttytab(X) :- tab(user, X).

ttyskip(X) :- skip_code(user, X).

ttyflush :- flush_output(user).

ttyget(N) :- get_code(user, N).

ttyget1(N) :- get1_code(user, N).

ttynl :- nl(user).

ttyskipeol :- skip_code(user, 0'
                           ).
ttydisplay(X) :- display(user, X).

ttydisplayq(X) :- displayq(user, X).

ttydisplay_string([]).
ttydisplay_string([X|Xs]) :-
        put_code(user, X),
        ttydisplay_string(Xs).

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:



