:- module(errhandle, [error_protect/1, handle_error/2], [assertions]).

:- meta_predicate(error_protect(goal)).

error_protect(Goal) :-
        intercept(Goal, error(Error,Where), handle_error(Error, Where)).

handle_error(Error, Where) :-
        current_output(S),
        set_output(user_error),
        display('{ERROR: '),
        display_where(Where),
        display(' - '),
        display_error(Error),
        display('}'),
        nl,
        set_output(S),
        fail.

display_where(P/N-A) :- !,
        display_list([P,'/',N,', arg ',A]). 
display_where(P/N) :- !,
        display_list([P,'/',N]). 
display_where(W) :-
        display(W).

display_error(instantiation_error) :- !,
        display('instantiation error').
display_error(existence_error(procedure,_P)) :- !,
        display('undefined predicate').
display_error(existence_error(source_sink, S)) :- !,
        display_list(['file ',S,' not found']).
display_error(permission_error(open, source_sink, S)) :- !,
        display_list(['cannot open file ',S]).
display_error(permission_error(input,stream,S)) :- !,
        display_list(['no input permission from ',S]).
display_error(permission_error(output,stream,S)) :- !,
        display_list(['no output permission to ',S]).
display_error(permission_error(input,past_end_of_stream,S)) :- !,
        display_list(['attempt to read past end of stream ',S]).
display_error(type_error(Type, Culprit)) :- !,
        display_list(['expected ',Type,', found ',Culprit]).
display_error(domain_error(Domain, Culprit)) :- !,
        display_list(['expected ',Domain,', found ',Culprit]).
display_error(syntax_error(L0,L1,Msg,ErrorLoc)) :- !,
        display('syntax error '),
        message_lns(message, L0, L1, ['\n',[](Msg),':']),
        message(ErrorLoc).
display_error(X) :-
        display(X).

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:


