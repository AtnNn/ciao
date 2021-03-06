:- package(dcg_phrase).
% (see dcg_phrase_doc for documentation)

:- use_module(engine(hiord_rt), [call/1]).
% Invoked from code generated by DCG translation
:- use_module(library(dcg/dcg_phrase_rt), [phrase/2, phrase/3]).

% The dcg_phrase package includes this callback in the module. This
% hook is used by the phrase/{2,3} predicates, which translate the
% input goal and call the expanded code in the context of the caller
% module.
:- export('\6\call_from_phrase'/1).
'\6\call_from_phrase'(X) :- call(X).
