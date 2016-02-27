:- use_module(library('persdb/persdbrt')).
:- load_compilation_module(library('persdb/persdbtr')).
:- add_sentence_trans(persistent_tr/2).
:- multifile('$is_persistent'/2).
:- meta_predicate('$is_persistent'(spec,?)).

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

