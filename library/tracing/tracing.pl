
:- load_compilation_module(library('tracing/tracing_expand')).

:- add_sentence_trans(expand_tracing/3).

% Defines spy/1 and nospy/1
% Adds expansion (after mine!)
:- include(library(byrdbox)).

:- use_module(library('tracing/traces')).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:
