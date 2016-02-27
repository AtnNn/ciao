:- module(statistics_hrtime,[],[assertions,foreign_interface]).
:- initialization(init_statistics_hrtime(true)).

:- true pred init_statistics_hrtime(go(Error)) :: int + (foreign, returns(Error)).

:- use_foreign_source(statistics_hrtime).
:- use_foreign_library(hrtime).

:- impl_defined([init_statistics_hrtime/1]).
