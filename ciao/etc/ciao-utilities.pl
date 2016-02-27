
:- use_package(assertions).

:- doc(filetype, part).

:- doc(title,"PART X - Miscellaneous standalone utilities").

:- doc(subtitle,"@bf{The Ciao System Documentation Series}").
:- doc(subtitle,"Technical Report CLIP X/98.1").
:- doc(subtitle,"@em{Draft printed on:} @today{}").

:- include(library('ClipAddress')).
:- include(library('Copyright')).

:- doc(summary,"@include{ciao-utilities.lpdoc}").

:- doc(module,"@include{ciao-utilities.lpdoc}").

%% This is a dummy definition of main to force documenter to produce 
%% application-type documentation (rather than library-type).

main.
