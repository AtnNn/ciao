:- module(autodoc_aux, [], [assertions, regtypes, basicmodes, fsyntax]).

:- doc(title, "Auxiliary Definitions").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jose F. Morales").

:- use_module(library(messages)).
:- use_module(library(system), [file_exists/1]).

:- use_module(lpdocsrc(src(autodoc_settings))).

% ---------------------------------------------------------------------------

:- export(all_vars/1).
% All elements of the list are variables
all_vars([]).
all_vars([H|T]) :- var(H), all_vars(T).

% ---------------------------------------------------------------------------

% TODO: replace by a predicate that opens a file, and closes it if
%       fails? call_cleanup?

:- export(read_file/2).
read_file(File, Content) :-
	file_exists(File),
	!,
	try_finally(
	    open(File, read, IS),
	    read_stream(IS, Content),
	    close(IS)
	).
read_file(File, []) :-
	error_message("file ~w not found", [File]).

read_stream(IS, Content) :-
	get_code(IS, N),
	( N = -1 ->
	    Content = []
	; Content = [N|Rest],
	  read_stream(IS, Rest)
	).

:- use_module(library(system_extra), [try_finally/3]).

% ---------------------------------------------------------------------------

:- export(ascii_blank_lines/2).
ascii_blank_lines(0,"") :- !.
ascii_blank_lines(N,[0'\n | R]) :-
	N1 is N-1,
	ascii_blank_lines(N1,R).

% ---------------------------------------------------------------------------
% (call external commands from lpdoc)

:- use_module(library(system_extra), [do/2, do/4, (-)/1]).
:- use_module(library(lists), [append/3, select/3]).

:- export(sh_exec/2).
% This command interfaces with do/{2,4} and includes several
% additional options:
%
%  - default: uses default options specified by get_command_options
%  - logbase(Base): specifies the file base for logs
%  - no_throw: catches exceptions (show warnings instead) (using (-)/1)
%  - bg: run in background
%
% TODO: This command could be simplified (probabily merged with system_extra)
% TODO: (-)/1 does not seem a good predicate name
sh_exec(Cmd, Opts0) :-
	% Expand options (@var{OptsN} is the new list of options)
	( select(default, Opts0, Opts1) ->
	    append(~get_command_option, OptsN1, OptsN)
	; OptsN = OptsN1, Opts0 = Opts1
	),
	%
	( select(logbase(Base), Opts1, Opts2) ->
	    LogBase = yes(Base), OptsN2 = OptsN1
	; LogBase = no, Opts1 = Opts2, OptsN2 = OptsN1
	),
	%
	( select(no_throw, Opts2, Opts3) ->
	    NoThrow = yes
	; NoThrow = no, Opts2 = Opts3
	),
	OptsN3 = OptsN2,
	%
	( select(bg, Opts3, Opts4) ->
	    Bg = yes
	; Bg = no, Opts3 = Opts4
	),
	OptsN4 = OptsN3,
	% Append remaining options
	OptsN4 = Opts2,
	%
	% TODO: ugly...
	( LogBase = yes(Base) ->
	    atom_concat(Base, '.log', LogFile),
	    atom_concat(Base, '.err', ErrFile)
	; true
	),
	( Bg = yes ->
	    append(Cmd, [' &'], Cmd2)
	; Cmd2 = Cmd
	),
	( NoThrow = yes ->
	    ( LogBase = yes(_) ->
	        -do(Cmd2, LogFile, ErrFile, OptsN)
	    ; -do(Cmd2, OptsN)
	    )
        ; ( LogBase = yes(_) ->
	      do(Cmd2, LogFile, ErrFile, OptsN)
	  ; do(Cmd2, OptsN)
	  )
	).

