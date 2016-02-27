:- module(tcltk,[],[assertions,isomodes,regtypes]).

%%----------------------------------------------------------------------
:- comment(title, "The tcl/tk interface").

:- comment(author,"Montse Iglesias").

:- export(tclInterpreter/1).
:- export(tclCommand/1).
:- export(tcl_new/1).
:- export(tcl_eval/3).
:- export(tcl_delete/1).
:- export(tcl_event/3).
:- export(tk_event_loop/1).
:- export(tk_new/2).
:- export(tk_new/4).

:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(engine(internals)).
:- use_module(library(write)).
:- use_module(library(strings)).

%%------------------------------------------------------------------------
:- comment(module,"The @lib{tcltk} library package is a bidirectional to
   the @em{Tcl} (pronounced Tickle) language and @em{Tk} toolkit. Tcl is an
   interpreter scripting language with many extensions packages, in particular
   the graphical interface toolkit Tk").
%%------------------------------------------------------------------------

:- regtype tclInterpreter(I) # "@var{I} is a reference to a @em{Tcl}
   interpreter.".

tclInterpreter(_).

:- pred tcl_new(-TclInterpreter) :: tclInterpreter # "Creates a new
   interpreter, initializes it, and returns a reference to it in
   @var{TclInterpreter}.".

tcl_new(I) :-
	tcltk_low_level:new_interp(I).

:- pred tcl_delete(+TclInterpreter) :: tclInterpreter # "Deletes the interpreter in
   @var{TclInterpreter} and the memory used by it".

tcl_delete(I) :-
	tcltk_low_level:delete(I).
	
:- pred tcl_eval(+TclInterpreter,+Command,-Result) 

	:: tclInterpreter * tclCommand * string

        # "Evaluates the commands and the result will be stored as a string in @em{Result}".

tcl_eval(I,Command,Result) :-
	tcltk_low_level:tcltk_raw_code("prolog_cmd {",I),
	tcltk_low_level:tcltk(Command,I),
	tcltk_low_level:tcltk_raw_code("}",I),
	tcltk_low_level:receive_term(Result,I).
%	tcltk_low_level:tcltk_error(Result,I).
      
:- pred tcl_event(+TclInterpreter,+Command,-Events)

	:: tclInterpreter * tclCommand * list

        # "Do the receive non blocking of the event and terms will be stored from Tcl by the @em{prolog_event} command as a list of terms in @em{Events}.".

tcl_event(I, _Command, EventList):-
	tcltk_low_level:receive_event(EventList,I).

%tcl_event(I,Command,TermList):-
%	tcltk_low_level:tcltk_raw_code("prolog_one_event {",I),
%	tcltk_low_level:tcltk(Command,I),
%	tcltk_low_level:tcltk_raw_code("}",I),
%	tcltk_low_level:receive_event(TermList,I).

%tcl_event(I,Command,EventList):-
%	tcl_eval(I,Command,_),
%	!,
%	tcltk_low_level:receive_event(EventList,I).


:- comment(tclCommand/1,"
	A @em{Tcl} command is specified as follows:
@begin{verbatim}
      Command         --> Atom  @{ other than [] @}
                        | Number
                        | chars(PrologString)
                        | write(Term)
                        | format(Fmt,Args)
                        | dq(Command)
                        | br(Command)
                        | sqb(Command)
                        | min(Command)
                        | ListOfCommands
      ListOfCommands  --> []
                        |[Command|ListOfCommands]
@end{verbatim}

where:

@begin{description}

@item{Atom}

@item{Number} denote their printed representations.

@item{chars(PrologString)} denotes the string represented by
     @em{PrologString} (a list of character codes).

@item{write(Term)} denotes the string that is printed by the corresponding 
     built-in pridicate.

@item{format(Term)} denotes the string that is printed by the corresponding 
     built-in pridicate.

@item{dq(Command)} denotes the string specified by 
     @em{Command}, enclosed in double quotes.

@item{br(Command)} denotes the string specified by 
     @em{Command}, enclosed in braces.

@item{sqb(Command)} denotes the string specified by 
     @em{Command}, enclosed in square brackets.

@item{min(Command)} denotes the string specified by 
     @em{Command}, immediately preceded by a hyphen.

@item{ListOfCommands} denotes the strings denoted by each element, separated 
     by spaces. 

@end{description}

").

:- regtype tclCommand(C) # "@var{C} is a @em{Tcl} command.".

tclCommand(_).

%%-------------------------------------------------------------
%%  TK
%%-------------------------------------------------------------

:- pred tk_new(+Options,-TclInterpreter)

	:: list * tclInterpreter

        # "Performs basic Tcl and Tk initialization and creates the main window of a Tk application.@em{Options} is a list of optional elements according to :

@begin{description}

@item{name} Sets the Tk application name. The application name will be displayed in the main window and is also used for communicating between applications in Tk. Default name is an empty string.

@item{display} Gives the name of the screen on which to create the main window. Default is normally determined by the DISPLAY environment variable.

@item{file} Open the sript file, commands will not be read from standard input and the execution returns back to Prolog only after all windows (and the interpreter) have been deleted.

@end{description}

".


%tk_new([],Interp):-
%	tcl_new(Interp).
tk_new(Options, Interp):-
	tk_options(Options,_,Appname,_,Display,_,File),
	!,
%	tcl_new(Interp),
	tk_new(Interp,Appname,Display,File).


tk_options(Option,_,_,_,_,_,_):-
	var(Option),
	!,
	fail.
tk_options([],App,App,Disp,Disp,File,File).
tk_options([Option|Options],App0,App,Disp0,Disp,File0,File):-
	nonvar(Option),
	tk_option(Option,App0,App1,Disp0,Disp1,File0,File1),
	tk_options(Options,App1,App,Disp1,Disp,File1,File).

tk_option(file(File),App0,App,Disp0,Disp,_,Filename):-
	App=App0, 
	Disp=Disp0,
	Filename = File.
tk_option(name(Name),_,App,Disp0,Disp,File0,File):-
	App=Name,
	Disp=Disp0,
	File=File0.
tk_option(display(Display),App0,App,_,Disp,File0,File):-
	App=App0,
	Disp=Display,
	File=File0.

% Hay que poner en la segunda condicion del if que si no hay display
tk_new(Interp,Appname,Display,File):-
	(nonvar(Appname)->atom_concat(' -name ',Appname,Str1);
	    atom_concat(' ',' ',Str1)),
	(nonvar(Display)->atom_concat(' -display ',Display,Str2);
%	    atom_concat(' -display ',Display,Str2)),
            atom_concat(' ',' ',Str2)),
	(nonvar(File)->atom_concat(' ',File,Str3);
            atom_concat(' ',' ',Str3)),
	atom_concat(Str1,Str2,Str4),
	atom_concat(Str4,Str3,Options),
	tcltk_low_level:new_interp(Interp,Options).

:- meta_predicate tk_event_loop(addmodule).

:- pred tk_event_loop(+TclInterpreter,+Module)

	:: tclInterpreter * list

        # "Passes control to Tk until all windows are gone.".


tk_event_loop(X,FromModule):-
	tcl_event(X,[],Event),
	( member(execute(Goal),Event) -> 
	  (
	   catch(do_call(Goal,FromModule),Error,tcl_loop_exit(X,Error)),
	   tcltk_low_level:send_term(Goal,X),
%           display(Goal),
%	   tcl_eval(X,[set,'Outputval',6],_),
	   tk_event_loop(X,FromModule))
%             (do_call(Goal,FromModule), tk_event_loop(X,FromModule)) 
	     ;
	    true
%	     tcl_delete(X) 
	).

%predicate used to exit closing the main window
tcl_loop_exit(X,E):-
	write('Prolog exception: '),
	write(E),
	tcl_delete(X),
	fail.

do_call(Module:Goal,_FromModule) :-
%	last_module_exp(Module:Goal,goal,FromModule,_,NewGoal),
	module_concat(Module,Goal,NewGoal),
	!,
	'$meta_call'(NewGoal).

%do_call(Goal,FromModule) :-
%	functor(Goal,F,A),
%	imports(FromModule,Defines,F,A),
%	display('imports'),
%	module_concat(Defines,Goal,NewGoal),
%	!,
%	'$meta_call'(NewGoal).

do_call(Goal,FromModule) :-
	module_concat(FromModule,Goal,NewGoal),
	!,
	'$meta_call'(NewGoal).


:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+73,2000/03/19,19:54*31+'CET'), "Added this new
   version of @lib{tcltk} to distribution. Not tested too much yet.
   (Manuel Hermenegildo)").

:- comment(version(1*5+16,1999/12/14,17:12*49+'MET'), "Fixed minor bug
   in documentation.  (Manuel Hermenegildo)").

:- comment(version(0*9+85,1999/05/07,19:55*11+'MEST'), "Started
   documentation.  (Montse Urraca)").

:- comment(version(0*9+84,1999/05/07,19:54*57+'MEST'), "Some bugs fixed.
   (Montse Urraca)").

:- comment(version(0*9+83,1999/05/07,19:54*21+'MEST'), "Some variables
   anonimized. (Manuel Carro)").

:- comment(version(0*9+82,1999/05/07,19:53*49+'MEST'), "Synchronized file
   versions with global CIAO version.  (Manuel Carro)").

