:- module(streams, [
        open_null_stream/1,
        open_input/2, close_input/1, open_output/2, close_output/1
        ],[assertions]).

:- use_module(engine(internals)).

:- comment(title,"Structured stream handling").

open_null_stream(S) :-
	'$open'('/dev/null', w, S).

open_input(FileName, (OldInput, NewInput)) :-
        current_input(OldInput),
        open(FileName, read, NewInput),
        set_input(NewInput).

close_input((OldInput, NewInput)) :-
        set_input(OldInput),
        close(NewInput).

open_output(FileName, (OldOutput, NewOutput)) :-
        current_output(OldOutput),
        open(FileName, write, NewOutput),
        set_output(NewOutput).

close_output((OldOutput, NewOutput)) :-
        set_output(OldOutput),
        close(NewOutput).

:- comment(version(0*5+15,1998/06/09,16:30*53+'MET DST'), "Added
   @{open,close@}_@{input,output@}. (Daniel Cabeza Gras)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

