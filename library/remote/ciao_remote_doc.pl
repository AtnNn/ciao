:- module(ciao_remote_doc,
        [
 %%             (@)/2,
 %%              server_stop/1,
 %%              server_trace/1,
 %%              server_notrace/1,
 %%              serve/0
        ], [assertions]).


 %% :- use_module(library('remote/ciao_server_rt'), [serve/0]).
 %% :- reexport(library('remote/ciao_server_rt'), [serve/0]).

:- use_module(library('remote/ciao_client_rt'),
        [(@)/2, server_stop/1, server_trace/1, server_notrace/1]).
:-   reexport(library('remote/ciao_client_rt'),
        [(@)/2, server_stop/1, server_trace/1, server_notrace/1]).

:- comment(title, "The Ciao Remote Services Package").

:- comment(author, "Manuel Carro").

:- comment(summary,
"This package implements basic services to allow users to call remote Ciao
Prolog servers.  Remote servers are processes living in a separate process,
possibly in another machine, which offer @concept{predicates} as
services.  This package makes it easy:

@begin{itemize}
@item To use a regular Ciao Prolog module as a service in the server.
@item To call a remotely-served predicate from any Ciao Prolog module.
@end{itemize}

Unification of variables and attribute encoding/decoding (and thus
constraint passing and predicate delay) are automatically handled by
the package (see the examples in the 'examples' subdirectory).
Concurrency, local and remote, can be used freely.").


:- comment(bug, "Dynamic loading of code not yet implemented.").
:- comment(bug, ":- remote/1 predicate declaration not yet implemented.").
:- comment(bug, "Remote use of modules (http, ftp, ciaotp) not yet implemented.").
:- comment(bug, "Remote creation of objects not yet implemented.").
:- comment(bug, "Code migration not yet implemented (several algorithms possible).").
:- comment(bug, "Evaluation of impact of marshalling and/or attribute encoding not yet done.").
:- comment(bug, "Secure transactions not yet implemented.").


:- comment(version_maintenance,dir('../../version')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*7+17,2000/09/11,16:58*15+'CEST'), "First stab at
the remote server.  (MCL)").

