%
% example to show low-level java interface capabilities.
% On this example can be seen the following features:
%  - java object creation
%  - java method invocation
%  - handling of java events from prolog
%
% This example shows a calculator pad that displays on the calculator
% screen the keys clicked. On the java side are used the jdk standard
% classes, controlled by the prolog side. The action events are also
% controlled by the prolog side via event-handling predicates, posted
% on the java event queue listener through the interface primitive
% java_add_listener/3.
%
% To run this example, just use this module from the top-level and
% type example.
%

:- module(benchmark,[]).

:- use_module(library('javall/javart')).
:- use_module(library(lists)).
:- use_module(library(prolog_sys)).
:- export(benchmark/0).
:- export(main/0).

:- dynamic connected/1.

main:-
	benchmark.

benchmark:-
	(connected(_) -> 
	 true
	;
	 javart:java_connection,
	 assert(connected(clip))
	),

	bench_strings(1000,TimeStrings),

%	bench_frames(1000,TimeFrames),

	display('Time creating strings:'),display(TimeStrings),nl
%	display('Time creating Frames: '),display(TimeFrames),nl
        .

bench_strings(N,Time) :-
	elapsed_time(_),
	bench_strings(N),
	elapsed_time(Time).

bench_strings(0).
	
bench_strings(N) :-
	javart:java_create_object('java.lang.String'('Prueba'),_Str),
	N1 is N - 1,
	bench_strings(N1).

%	javart:java_create_object('java.awt.Frame'('Prueba'),Frame),



elapsed_time(T) :-
	statistics(walltime,[_,T]).
