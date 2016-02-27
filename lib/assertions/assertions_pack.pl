
:- new_declaration(comment/2).
:- comment(hide,comment/2). % DOES NOT WORK!

:- new_declaration(modedef/1).         :- op(1150, fx,(modedef)).
:- comment(hide,modedef/2). % DOES NOT WORK!

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:
