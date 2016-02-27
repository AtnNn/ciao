:- module(_, [main/0], []).

% TODO: Disabled, class is a package, not a module.
%       Why was it necessary? (JFMC)
%:- ensure_loaded(library(class)).

%% Force creation of class.itf and class.asr by having this fake module 
%% which uses it.

main.
