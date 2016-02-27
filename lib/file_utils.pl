:- module(file_utils, [file_terms/2, copy_stdout/1, 
	  file_to_string/2,stream_to_string/2],
        [assertions,isomodes]).

:- use_module(library(read), [read/1]).
:- use_module(library(streams)).

:- comment(title,"File I/O utilities").

:- comment(author,"The CLIP Group").

:- comment(module,"This module implements the file I/O utilities.").

:- pred file_terms(@File, ?Terms) => sourcename * list 
   # "Transform a file @var{File} to/from a list of terms @var{Terms}.".

:- pred file_terms(File, Terms) : sourcename * var => sourcename * list 
   # "Unifies @var{Terms} with the list of all terms in @var{File}.".

:- pred file_terms(File, Terms) : sourcename * list => sourcename * list 
   # "Writes the terms in list @var{Terms} (including the ending '.')
      onto file @var{File}.".

file_terms(File, Terms) :- var(Terms), !,
        open_input(File, IO),
        read(T),
        read_terms(T, Terms),
        close_input(IO).
file_terms(File, Terms) :-
        open_output(File, IO),
        display_term_list(Terms),
        close_output(IO).        

read_terms(end_of_file, []) :- !.
read_terms(T, [T|Ts]) :-
        read(T1),
        read_terms(T1, Ts).

display_term_list([]).
display_term_list([T|Ts]) :-
        display_term(T),
        display_term_list(Ts).

:- pred copy_stdout(+File) => sourcename 
   # "Copies file @var{File} to standard output.".

copy_stdout(File) :-
 	open_input(File, IO),
	repeat,
	  get_code(Code),
	  ( Code = -1
	  ; put_code(Code),
	    fail
	  ),
	!,
	close_input(IO).

:- pred file_to_string(+FileName, -String) :: sourcename * string
   # "Reads all the characters from the file @var{FileName}
      and returns them in @var{String}.".

file_to_string(File, String) :-
        open(File, read, Stream),
        stream_to_string(Stream, String).

:- pred stream_to_string(+Stream, -String) :: stream * string
   # "Reads all the characters from @var{Stream}
      and returns them in @var{String}.".

stream_to_string(Stream, String) :-
        current_input(OldIn),
        set_input(Stream),
        read_to_close(String),
        set_input(OldIn),
        close(Stream).

read_to_close(L) :-
        get_code(C),
        read_to_close1(C, L).

read_to_close1(-1, []) :- !.
read_to_close1(C, [C|L]) :-
        get_code(C1),
        read_to_close1(C1, L).

%---------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*9+216,2003/12/21,02:30*59+'CET'), "Added comment
   author and module.  (Edison Mera)").

:- comment(version(1*5+55,2000/02/11,21:19*43+'CET'), "Changed
   file_to_string/2 and stream_to_string/2 which did not work well
   (Daniel Cabeza Gras)").

:- comment(version(1*5+53,2000/02/10,20:27*23+'CET'), "Added
   stream_to_string/2. (This library needs serious extension/cleaning
   up).  (Manuel Hermenegildo)").

:- comment(version(0*5+17,1998/06/11,21:05*03+'MET DST'), "Added
   file_to_string/2, fixed bug in copy_stdout/1 (Daniel Cabeza Gras)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global Ciao version.  (Manuel Hermenegildo)").

%---------------------------------------------------------------------------
