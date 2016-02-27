:- module(tcltk,[],[assertions,isomodes,regtypes]).

%%----------------------------------------------------------------------
:- comment(title, "The Tcl/Tk interface").

:- comment(author,"Montse Iglesias Urraca").
:- comment(author, "@tt{http://www.clip.dia.fi.upm.es/}").
:- comment(author, "The CLIP Group").
:- comment(author, "Facultad de Inform@'{a}tica").
:- comment(author, "Universidad Polit@'{e}cnica de Madrid").

:- export(tclInterpreter/1).
:- export(tclCommand/1).
:- export(tcl_new/1).
:- export(tcl_eval/3).
:- export(tcl_delete/1).
:- export(tcl_event/3).
%:- export(tcl_event_bueno/3).
:- export(tk_event_loop/1).
:- export(tk_loop/1).
:- export(tk_new/2).
%:- export(tk_new/4).
:- export(tk_next_event/2).


:- use_module(library('tcltk/tcltk_low_level')).
:- use_module(engine(internals)).
:- use_module(library(write)).
:- use_module(library(strings)).
:- use_module(library(lists),[append/3,list_insert/2]).

:- set_prolog_flag(multi_arity_warnings, off).

%%------------------------------------------------------------------------
:- comment(copyright,"@include{Copyright.Manuals}").

:- comment(summary,
        "This document includes the reference manual of the Prolog Tcl/Tk bidirectional interface implemented in Ciao. This library of prolog predicates allows connection between @em{Tcl/Tk} graphical interface and @em{Prolog} programs.").

:- comment(module,"The @lib{tcltk} library package is a bidirectional interface to
   the @em{Tcl} (pronounced Tickle) language and @em{Tk} toolkit. Tcl is an
   interpreter scripting language with many extensions packages, in particular
   the graphical interface toolkit Tk.
The proposed interaction between both languages is realized as an interface between two process, a Tcl/Tk process and a Prolog process. This approach allows programmers to use both Tcl/Tk and Prolog.

    @section{Prolog - Tcl/Tk interface structure}
    The interface is made up of two parts: a Prolog part and a Tcl/Tk part. The Prolog part receives requests from a Prolog program and sends them to the Tcl/Tk part. The Tcl/Tk part receives from the socket and performs the actions included in the requests.

    @subsection{Prolog side}
    The Prolog side receives the actions to do in the Tcl/Tk side from the user program, and sends them to the Tcl/Tk side through the socket connection. When the action is done in the Tcl/Tk side, the result is retrieved to the user program, or the action fails if any problem sucess. 

    @subsection{Tcl/Tk side}
    The Tcl/Tk side waits for requests from the Prolog side, executes the Tcl/Tk code sended from the Prolog side, handle the events and exceptions raised in the Tcl/Tk side.
").
%%------------------------------------------------------------------------

:- regtype tclInterpreter(I) # "@var{I} is a reference to a @em{Tcl}
   interpreter.".

tclInterpreter(_).

%%------------------------------------------------------------------------
:- pred tcl_new(-TclInterpreter) :: tclInterpreter # "Creates a new
   interpreter, initializes it, and returns a reference to it in
   @var{TclInterpreter}.".
%%------------------------------------------------------------------------

tcl_new(I) :-
        tcltk_low_level:new_interp(I).

%%------------------------------------------------------------------------
:- pred tcl_delete(+TclInterpreter) :: tclInterpreter # "Given a handle to a Tcl interpreter in variable @var{TclInterpreter}, it deletes the interpreter from the system.".
%%------------------------------------------------------------------------

tcl_delete(I) :-
        tcltk_low_level:delete(I).

%%------------------------------------------------------------------------
:- pred tcl_eval(+TclInterpreter,+Command,-Result) 

        :: tclInterpreter * tclCommand * string

        # "Evaluates the commands given in variable @var{Command} in the Tcl 
          interpreter in variable @var{TclInterpreter}. The result will be stored 
          as a string 
          in @var{Result}. If there is an error in the @em{Command} an exception
          is raised. The error messages will be @em{Tcl Exception: } if the error
          is in the syntax of the tcltk code or @em{Prolog Exception: }, if the 
          error is in the prolog term.".
%%------------------------------------------------------------------------
%:- export(tcl_eval_result/1).

:- meta_predicate tcl_eval_result(X,addmodule).

:- impl_defined(tcl_eval_result/2).

tcl_eval_result(X,Result,FromModule) :-
        ( member(execute(Goal),[Result]) -> 
          (
              catch(do_call(Goal,FromModule),Error,tcl_loop_exit(X,Error) ),
              tcltk_low_level:send_term(Goal,X)
          )
             ;
            true
        ).

tcl_eval(I,Command,Result) :-
        tcltk_low_level:tcltk_raw_code("prolog_cmd {",I),
        tcltk_low_level:tcltk(Command,I),
        tcltk_low_level:tcltk_raw_code("}",I),
%       tcltk_low_level:receive_term(Result,I), display(Result),nl.
        tcltk_low_level:receive_result(Result,I),
        tcl_eval_result(I,Result).
%       tcltk_low_level:tcltk_error(Result,I).


%%------------------------------------------------------------------------
%:- pred tcl_event(+TclInterpreter,+Command,-Events)

%       :: tclInterpreter * tclCommand * list

%        # "Do the receive non blocking of the event and terms will be stored from Tcl by the @em{prolog_event} command as a list of terms in @em{Events}.".
%%------------------------------------------------------------------------

%tcl_event(I, _Command, EventList):-
%       tcltk_low_level:receive_event(EventList,I).

%%------------------------------------------------------------------------
:- pred tcl_event(+TclInterpreter,+Command,-Events)

        :: tclInterpreter * tclCommand * list

        # "Evaluates the commands given in @var{Command} in Tcl interpreter handle
          provided in @var{TclInterpreter}. @var{Events} is a list of terms stored 
          from Tcl by the @em{prolog_event}. Blocks until there is something on the
          event queue".
%%------------------------------------------------------------------------

%tcl_event(I,[],EventList):-
%       display('En el tcl_event1'),nl,
%       tcltk_low_level:tcltk_raw_code("prolog_list_events ",I),
%       tcltk_low_level:receive_list(EventList,I),
%       display('En el fin tcl_event'),nl.

tcl_event(I,Command,EventList):-
%       display('En el tcl_event1'),nl,
        tcltk_low_level:tcltk(Command,I),
        tcltk_low_level:tcltk_raw_code("prolog_list_events ",I),
        tcltk_low_level:receive_list(EventList,I).
%       display('En el fin tcl_event'),nl.

%tcl_event(I,Command,EventList):-
%       tcl_eval(I,Command,_),
%       !,
%       tcltk_low_level:receive_event(EventList,I).

%%------------------------------------------------------------------------
:- comment(tclInterpreter/1,"
        To use Tcl, you must create a @em{Tcl interpreter} object and send commands to it.").

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
%%------------------------------------------------------------------------
:- regtype tclCommand(C) # "@var{C} is a @em{Tcl} command.".

tclCommand(_).

%%-------------------------------------------------------------
%%  TK
%%-------------------------------------------------------------
%%------------------------------------------------------------------------
:- pred tk_new(+Options,-TclInterpreter)

        :: list * tclInterpreter

        # "Performs basic Tcl and Tk initialization and creates the main window of a Tk application.@var{Options} is a list of optional elements according to :

@begin{description}

@item{name(+ApplicationName)} Sets the Tk main window title @var{ApplicationName}. It is also used for communicating between Tcl/Tk applications via Tcl @em{send} command. Default name is an empty string.

@item{display(+Display)} Gives the name of the screen on which to create the main window. Default is normally determined by the DISPLAY environment variable.

@item{file} Opens the sript file. Commands will not be read from standard input and the execution returns back to Prolog only after all windows (and the interpreter) have been deleted.

@end{description}

".
%%------------------------------------------------------------------------

%tk_new([],Interp):-
%       tcl_new(Interp).
tk_new(Options, Interp):-
        tk_options(Options,_,Appname,_,Display,_,File),
        !,
%       tcl_new(Interp),
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
%           atom_concat(' -display ',Display,Str2)),
            atom_concat(' ',' ',Str2)),
        (nonvar(File)->atom_concat(' ',File,Str3);
            atom_concat(' ',' ',Str3)),
        atom_concat(Str1,Str2,Str4),
        atom_concat(Str4,Str3,Options),
        tcltk_low_level:new_interp(Interp,Options).

%%------------------------------------------------------------------------
:- pred tk_event_loop(+TclInterpreter) :: tclInterpreter # "Waits for
an event and executes the goal associated to it. Events are stored from Tcl with the 
@em{prolog} command. The unified term is
sent to the Tcl interpreter in order to obtain the value of the tcl
array of @em{prolog_variables}.  If the term received does not have
the form @tt{execute(Goal)}, the predicate silently exists.  If the
execution of @var{Goal} raises a Prolog error, the interpreter is
deleted and an error message is given.".

%%------------------------------------------------------------------------

:- meta_predicate tk_event_loop(addmodule).

tk_event_loop(X,FromModule):-
        tcltk_low_level:receive_event(Event,X),
%        display(Event), nl,
 ( 
        member(execute(Goal), Event) -> 
        (
            Goal = exit_tk_event_loop ->       % Leave event loop
            tcl_delete(X),
            true
        ;
            (
                Goal = exit_tk_event_loop(G1) ->
                tcltk_low_level:send_term(Goal,X),
                tcl_delete(X),
%               catch(do_call(G1,FromModule),Error,tcl_loop_exit(X,Error)),
                catch(do_call(G1,_FromModule),Error,_), true
            ;
                catch(do_call(Goal,FromModule),Error,tcl_loop_exit(X,Error)),
                tcltk_low_level:send_term(Goal,X),
                tk_event_loop(X,FromModule)
            )
        )
  %   (do_call(Goal,FromModule), tk_event_loop(X,FromModule)) 
 ;      %% Unknown command --- raise exception
        %     throw(unknown_command_in_tk_event_loop(X,FromModule))
        ( 
            Event = [end_of_file] ->
            true
        ;
            unknown_command_in_tk_event_loop(X)
        )
 ).

unknown_command_in_tk_event_loop(X) :-
        write('Prolog exception: '),
        write('The term must have the form execute(goal)'),
        tcl_delete(X),
        fail.

%predicate used to exit closing the main window
tcl_loop_exit(X,E):-
        write('Prolog exception: '),
        write(E),
        tcl_delete(X),
        fail.

do_call(Module:Goal,_FromModule) :-
%       last_module_exp(Module:Goal,goal,FromModule,_,NewGoal),
        module_concat(Module,Goal,NewGoal),
        !,
        '$meta_call'(NewGoal).

%do_call(Goal,FromModule) :-
%       functor(Goal,F,A),
%       imports(FromModule,Defines,F,A),
%       display('imports'),
%       module_concat(Defines,Goal,NewGoal),
%       !,
%       '$meta_call'(NewGoal).

do_call(Goal,FromModule) :-
        module_concat(FromModule,Goal,NewGoal),
        !,
        '$meta_call'(NewGoal).

%%------------------------------------------------------------------------
:- pred tk_next_event(+TclInterpreter,-Event) 
        
        :: tclInterpreter * string 
        
        # "Processes events until there is at least one Prolog event associated
          with @var{TclInterpreter}. @var{Event} is the term correspondig to the 
          head of a queue of events stored from Tcl with the @em{prolog_event}
          command.".
%%------------------------------------------------------------------------

tk_next_event(X,Event) :-
        tcl_event(X,[],[Event1|_]),
        Event1 == end_of_event_list,!,
        tk_next_event(X,Event).

tk_next_event(X,Event) :-
        tcl_event(X,[],[Event|_]),
        tcltk_low_level:tcltk_raw_code("prolog_delete_event ",X),
        tcltk_low_level:receive_confirm(_,X).
        
%       display('En tk_next_event'),nl,
%       tcl_event(X,[],[Event1|_]),
%       ( Event1 = end_of_event_list -> tk_next_event(X,_)
%         ; 
%       Event = Event1,
%%      display('Despues tcl_event '),display(Event),nl,
%       tcltk_low_level:tcltk_raw_code("prolog_delete_event ",X),
%%      tcltk_low_level:delete_item_queue(X),
%%      display('Despues prolog_delete '),display(Event),nl,
%       tcltk_low_level:receive_confirm(_,X),display(Event),nl).
%%      display('En el fin tk'),nl.
%%      tcltk_low_level:tcltk_raw_code("prolog_delete_event ",X).

tk_next_event(_,_).
%%------------------------------------------------------------------------
:- pred tk_loop(+TclInterpreter) 
        
        :: tclInterpreter  
        
        # "Passes control to Tk until all windows are gone.".
%%------------------------------------------------------------------------

:- meta_predicate tk_loop(addmodule).

tk_loop(X,FromModule):-
        tk_next_event(X,Event),
        ( member(end_of_event_list,[Event]) -> 
          ( tk_loop(X,FromModule) )
        ;
            catch(do_call(Event,FromModule),Error,tcl_loop_exit(X,Error)),
            tk_loop(X,FromModule)
        ).


:- comment(version_maintenance,dir('../../version')).

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


