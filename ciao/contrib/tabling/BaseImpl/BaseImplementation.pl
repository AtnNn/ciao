:- package('BaseImplementation').

:- use_package(assertions).

%% Prevent documenter from doing anything with previous package
:- doc(nodoc, assertions).

%% File type not strictly needed (package is default for non-module
%% files), but just in case things change 
:- doc(filetype, package).

:- doc(title,"Tabling").

:- doc(author,"The CLIP Group").

:- doc(usage, "The TABLED_EXECUTION flag must be set to \"yes\"
                   in order to compile the engine with support for the
                   tabling execution of goals.").

:- op(1150, fx, [ table ]).

:- load_compilation_module(library(tabling('BaseImplementation'(tabling_tr_Ramesh)))).

:- add_sentence_trans(do_term_expansion/3).

:- use_module(library(tabling('BaseImplementation'(tabling_rt_Ramesh)))).
