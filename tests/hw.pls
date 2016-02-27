#!/bin/sh
exec ciao-shell $0 "$@" # -*- mode: ciao; -*-

%% Script version of hello world. 
%% The first two lines are necessary in Un*x (and skipped by Windows).

main(_) :- 
	write('Hello world!'), nl, nl,
	write('Hit return to proceed... '), 
	flush_output,
	get_code(_).

