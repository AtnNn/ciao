:- module(file_utils, [file_terms/2, copy_stdout/1, file_to_string/2],
        [assertions,isomodes]).

:- use_module(library(read), [read/1]).
:- use_module(library(streams)).

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
        open_input(File, IO),
        get_code(Code),
        get_string(Code, String),
        close_input(IO).

get_string(-1, []) :- !.
get_string(C, [C|Cs]) :-
        get_code(D),
        get_string(D, Cs).

:- comment(version(0*5+17,1998/06/11,21:05*03+'MET DST'), "Added
   file_to_string/2, fixed bug in copy_stdout/1 (Daniel Cabeza Gras)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
