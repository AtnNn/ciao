:- module(md5sum, [md5sum/2], [assertions]).

:- doc(title, "MD5 Message-Digest Algorithm (md5sum)").

:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Jose F. Morales (minor changes)").
:- doc(author, "The CLIP Group").

:- doc(module, "This module provides a predicate to compute the
   @href{http://en.wikipedia.org/wiki/MD5}{MD5 checksum} of a file, by
   invoking the external command @apl{md5sum} of @apl{md5} (depending
   on the OS).").

:- use_module(library(system)).
:- use_module(engine(system_info), [get_os/1]).
:- use_module(library(lists), [length/2, append/3]).

:- doc(bug, "Encode the algorithm here or link to external libraries?").
:- doc(bug, "Move with other hash functions.").

% TODO: Move to a library
:- pred md5sum(File, CheckSum) 
   # "Unifies @var{CheckSum} with the MD5 checksum of the file
      specified by @var{File}. @var{CheckSum} will be a string of 32
      hexadecimal codes.".

%:- pred md5sum(File, Result) : string => string.

md5sum(File, Result) :-
	% TODO: This can be resolved at compile time
	get_os(OS),
	( OS = 'DARWIN' ->
	    Cmd = 'md5', Args = ['-r', File]
	; Cmd = 'md5sum', Args = [File]
	),
	%
	exec(Cmd, Args, In, Out, Err, wait, _Pid, ErrCode),
	( ErrCode == 0,
	  read_codes(Out, Result1),
	  length(Result, 32),
	  append(Result, _, Result1),
	  check_hexa(Result) ->
	    close(Out), close(Err), close(In),
	    true
	; repeat_str(Err),
	  close(Out), close(Err), close(In),
	  fail
	).

read_codes(Str, Result) :-
	get_code(Str, C),
	(
	    (C = -1; C=10) ->
	    Result = []
	;
	    Result = [C|T],
	    read_codes(Str, T)
	).

repeat_str(Str) :-
	get_code(Str, C),
	(
	    C = -1 ->
	    true
	;
	    put_code(C),
	    repeat_str(Str)
	).

check_hexa([H|T]) :-
	( H >= 0'0, H =< 0'9 ;
	  H >= 0'A, H =< 0'F ;
	  H >= 0'a, H =< 0'f
	), !,
	check_hexa(T).
check_hexa([]).

