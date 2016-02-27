% UNDER DEVELOPMENT
% ----------------
% Prolog server used in the Java to Prolog interface
% examples.
%
% This program just redirects standard output and
% standard error streams to plserver.out and plserver.err
% respectively, and starts the prolog
% server.

:- use_module(library('javall/jtopl')).
:- use_module(library('format')).
:- use_module(library('system'),[datime/1]).

main :-	
%	open('plserver.out', append, Stdout),
%	current_output(CO),
%	set_output(Stdout),
%	system:datime(datime(Year, Month, Day, Hour, Minute, Second)),
%	format('~p/~p/~p - ~p:~p:~p', [Year, Month, Day, Hour, Minute, Second]),
%	display(' - plserver standard output log file'), 
%	nl,
	prolog_server.
%	flush_output,
%	set_output(CO),
%	close(Stdout)


