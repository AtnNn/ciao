:- package(rtchecks_tr_inline).
:- use_package(inliner).

% We use append/3 and select/3 inlined to avoid problems if the list
% module is compiled with run-time checks
:- inline_module(library(lists), [append/3, reverse/2, reverse/3,
		select/3, difference/3, intersection/3]).

:- unfold collect_checks(no, yes, no).
:- inline_module(library(rtchecks(rtchecks_meta))).
