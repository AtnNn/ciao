:- use_package([assertions]).  
:- use_module(engine(internals)).

:- comment(title,"Printing the contents of a bytecode file").

main(['-h']) :-
	usage.
main([File]) :-
	viewql(File).
main(Args) :-
	inform_user(['error: invalid arguments ',Args]),
	nl(user_error),
	usage.

usage :-
	usage_text(TextS),
	atom_codes(Text,TextS),
	inform_user(['Usage: ']),
	inform_user([Text]).

viewql(File) :-
        absolute_file_name(File, '_opt', '.po', '.', AbsName, _, _),
	'$push_qlinfo',
        '$open'(AbsName, read, Stream),            % Gives errors
	repeat,
	    '$qread'(Stream, Goal),
	    (   Goal= -1
	    ;   display(Goal), nl, fail
	    ), !,
	'$pop_qlinfo',
	close(Stream).

usage_text("
	viewpo <file1>.po
	   : print .po contents in symbolic form

	viewpo -h
	   : print this information
").

:- comment(author,"Daniel Cabeza").

:- comment(module,"This simple program takes as an argument a bytecode
   (.po) file and prints out in symbolic form the information
   contained in the file. It uses compiler and engine builtins to do
   so, so that it keeps track with changes in bytecode format.

   @section{Usage (viewpo)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

   ").

:- comment(version(0*5+2,1999/11/11,19:20*50+'MET'), "Fixed usage
   message.  (Manuel Hermenegildo)").

:- comment(version(0*5+1,1998/1/27), "Added documentation. (Manuel
   Hermenegildo)").

:- comment(version(0*5+0,1997/10/12), "First version. (Daniel
   Cabeza)").

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

