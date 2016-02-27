%%------------------------------------------------------------------------
%% TCL/TK low level library
%%------------------------------------------------------------------------

:- module(tcltk_low_level,[],[assertions]).

:- use_module(library(terms)).

:- comment(title,
	"Low level interface library to Tcl/Tk").

:- export(new_interp/1).
:- export(new_interp/2).
:- export(tcltk/2).
:- export(tcltk_raw_code/2).
:- export(copy_stdin/1).
:- export(receive_term/3).
:- export(receive_term/2).
:- export(send_term/2). 

:- export(delete/1).
:- export(receive_event/2).
:- export(receive_list/2).
:- export(tcl_error/1).
%:- export(tcl_result/1).


:- use_module(library(sockets)).
:- use_module(library(system)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(strings)).
:- use_module(library(format),[format/3]).

%%-----------------------------------------------------------------------
:- comment(module,"The @lib{tcltk low level} library permit obtain the results of the 
   @lib{tcltk}").

%%-----------------------------------------------------------------------
%%------------------------------------------------------------------------
%% CONSTRUCTOR / DESTRUCTOR
%%------------------------------------------------------------------------

new_interp(X) :-
	nonvar(X),
	!,
	fail.

new_interp('$wish$'(Strm,TermStream,EventStream)) :-
	current_host(Host),
	bind_socket(Port,1,Socket),
	number_codes(Port,PortCode),
	popen('wish',write,Strm),
	write_string(Strm,"set prolog_host "),
	write(Strm,Host),nl(Strm),flush_output(Strm),
	write_string(Strm,"set prolog_port "),
	write_string(Strm,PortCode),nl(Strm),
	flush_output(Strm),
	bind_socket(EPort,1,ESocket),
	number_codes(EPort,EPortCode),
	write_string(Strm,"set event_port "),
	write_string(Strm,EPortCode),nl(Strm),
	flush_output(Strm),
	send_initial_code(Strm),
	socket_accept(Socket,TermStream),
	socket_accept(ESocket,EventStream),
	true.

:- export(new_interp_file/2).

new_interp_file(_,X) :-
	nonvar(X),
	!,
	fail.
new_interp_file(File,'$wish$'(Strm,TermStream,EventStream)) :-
	current_host(Host),
	bind_socket(Port,1,Socket),
	number_codes(Port,PortCode),
	atom_concat(['bltwish <',File,' &'],Command),
	popen(Command,write,Strm),
	write_string(Strm,"set prolog_host "),
	write(Strm,Host),nl(Strm),flush_output(Strm),
	write_string(Strm,"set prolog_port "),
	write_string(Strm,PortCode),nl(Strm),
	flush_output(Strm),
	bind_socket(EPort,1,ESocket),
	number_codes(EPort,EPortCode),
	write_string(Strm,"set event_port "),
	write_string(Strm,EPortCode),nl(Strm),
	flush_output(Strm),
	send_initial_code(Strm),
	socket_accept(Socket,TermStream),
	socket_accept(ESocket,EventStream),
	true.

new_interp('$wish$'(Strm,TermStream,EventStream),Options) :-
	current_host(Host),
	bind_socket(Port,1,Socket),
	number_codes(Port,PortCode),
	atom_concat('wishx',Options,V),
	popen(V,write,Strm),
	write_string(Strm,"set prolog_host "),
	write(Strm,Host),nl(Strm),flush_output(Strm),
	write_string(Strm,"set prolog_port "),
	write_string(Strm,PortCode),nl(Strm),
	flush_output(Strm),
	bind_socket(EPort,1,ESocket),
	number_codes(EPort,EPortCode),
	write_string(Strm,"set event_port "),
	write_string(Strm,EPortCode),nl(Strm),
	flush_output(Strm),
	send_initial_code(Strm),
	socket_accept(Socket,TermStream),
	socket_accept(ESocket,EventStream),
	true.

delete('$wish$'(Strm,TermStrm,EventStrm)) :-
	write_string(Strm,"uplevel 0 exit"),
	nl(Strm),
	flush_output(Strm),
	close(TermStrm),
	close(Strm),
	close(EventStrm).

%%------------------------------------------------------------------------

send_initial_code(Strm) :-
	core(String),
	write_string(Strm,String),
	nl(Strm),
	flush_output(Strm),
	fail.

send_initial_code(_).

%%------------------------------------------------------------------------
%% SEND BASIC TCLTK CODE ITEMS TO WISH
%%------------------------------------------------------------------------

tcltk_raw_code(Str,'$wish$'(Strm,_,_)) :-
	string(Str,String,""),
	!,
	write_string(Strm,String),
	nl(Strm),
	flush_output(Strm).

copy_stdin('$wish$'(Strm,_,_)) :-
	!,
	copy_stdin_aux(Strm).

copy_stdin_aux(Strm) :-
	get_code(Byte),
	Byte =\= -1,
	!,
	put_code(Strm,Byte),
	flush_output(Strm),
	copy_stdin_aux(Strm).

copy_stdin_aux(_).

%%------------------------------------------------------------------------
%% MACRO IN ORDER TO SEND TCL/TK CODE TO WISH
%%------------------------------------------------------------------------

tcltk(Code,'$wish$'(Strm,_,_)) :-
	!,
	nl(Strm),
	send_code(Code,Strm).

%%------------------------------------------------------------------------

send_code([],Strm) :-
	!,
	nl(Strm),
	flush_output(Strm).

send_code([Number|Nc],Strm) :-
	number(Number),
	!,
	number_codes(Number,NumberAsCode),
	write_string(Strm,NumberAsCode),
	write(Strm,' '),
	send_code(Nc,Strm).
	
send_code([chars(String)|Nc],Strm) :-
	!,
	send_code([String|Nc],Strm).

send_code([dq(Code)|Nc],Strm) :-
	write(Strm,'\"'),
	send_code(Code,Strm),
	write(Strm,'\" '),
	!,
	send_code(Nc,Strm).

send_code([sqb(Code)|Nc],Strm) :-
	write(Strm,'['),
	send_code(Code,Strm),
	write(Strm,'] '),
	!,
	send_code(Nc,Strm).

send_code([br(Code)|Nc],Strm) :-
	write(Strm,'{'),
	send_code(Code,Strm),
	write(Strm,'} '),
	!,
	send_code(Nc,Strm).

send_code([min(Code)|Nc],Strm) :-
	atom(Code),
	!,
	write(Strm,'-'),
	write(Strm,Code),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([format(Fmt,Args)|Nc],Strm) :-
	format(Strm,Fmt,Args),
	!,
	send_code(Nc,Strm).

send_code([write(Term)|Nc],Strm) :-
	write(Strm,Term),
	!,
	send_code(Nc,Strm).

send_code([tcl(Var)|Nc],Strm) :-
	number_codes(Var,Str),
	atom_codes(Atom,Str),
	write(Strm,Atom),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([Atom|Nc],Strm) :-
	atom(Atom),
	!,
	write(Strm,Atom),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([Str|Nc],Strm) :-
	string(Str,String,""),
	!,
	write_string(Strm,String),
	write(Strm,' '),
	send_code(Nc,Strm).

send_code([_|Nc],Strm) :-
	!,
	send_code(Nc,Strm).

send_code(Not_a_list,Strm) :-
	!,
	send_code([Not_a_list],Strm).

%%------------------------------------------------------------------------
%% SEND A PROLOG TERM TO TCL/TK
%%------------------------------------------------------------------------

send_term(Term,'$wish$'(_,Stream,_)) :-
        write_term(Stream,Term,[]),
	nl(Stream),flush_output(Stream).
	

%%------------------------------------------------------------------------
%% READ A PROLOG TERM FROM TCL/TK
%%------------------------------------------------------------------------

receive_term(Term,'$wish$'(_,Stream,_)) :-
        read_term(Stream,Term,[]),
	exceptions(Term).
	
receive_term(Term,VarNames,'$wish$'(_,Stream,_)) :-
        read_term(Stream,Term,[variable_names(VarNames)]).

%%------------------------------------------------------------------------
%% EXCEPTIONS FROM TCL/TK
%%------------------------------------------------------------------------

exceptions(Term) :- 
	catch(Term,Error,handle(Error)).

tcl_error(Text) :-
	write('Tcl exception: '),
	write_string(Text),
	nl,
	halt.

tcl_result(_).

handle(X) :-
	write(X),
	nl,
	halt.

%%________________________________________________________________________
%% READ A PROLOG LIST OF TERMS FROM TCLTK
%%________________________________________________________________________

receive_event([Term],'$wish$'(_,_,Stream)) :-
	read_term(Stream,Term,[]).


%%------------------------------------------------------------------------



receive_list([],'$wish$'(_,_,Stream)) :-
	!,
	read_term(Stream,end_of_event_list,[]).
	
receive_list([Term|Nt],'$wish$'(_,_,Stream)) :-
	read_term(Stream,Term,[]),
	!,
	receive_list(Nt,'$wish$'(_,_,Stream)).

%%------------------------------------------------------------------------
%% EVALUATE IF AN ERROR OCCURS IN THE TCLTK SCRIPT
%%------------------------------------------------------------------------



%%------------------------------------------------------------------------
%% INITIAL CODE
%%------------------------------------------------------------------------

%core("wm withdraw .").

core("set event_socket [socket $prolog_host $event_port]").
%core("puts event_socket").
%core("puts $event_socket").

core("set term_socket [socket $prolog_host $prolog_port]").
%core("puts term_socket").
%core("puts $term_socket").

core("[fconfigure event_socket -blocking true]").

%core(" global prolog_variables").
core("set prolog_variables(X) 1").

core("proc prolog_term {term} {").
core("global event_socket").
core(" puts  $event_socket $term. ").
core(" flush $event_socket ").
core("} ").

core("proc prolog {agoal} {").
%core(" prolog_term goal($agoal) ").
core(" prolog_term execute($agoal) ").
core("} ").

% Execute command and sendresults back to prolog
% This is internally used by tcl_eval/3.
% return 0 when the scripts runs without errors, and 1 if there is an error

core("proc prolog_cmd {command} {").
core(" global term_socket").
core("   set result [catch {uplevel $command} var]").
core("   if {$result} {").
core("       set var [convert_variable $var ]").
core("       puts  $term_socket tcltk_low_level:tcl_error(\\""$var\\"").").
core("       flush $term_socket").
core("       return $result").
core("   } else { ").
core("       puts  $term_socket tcltk_low_level:tcl_result(\\""$result\\"").").
core("       flush $term_socket").
core("       return $result").
core("   } ").
core("} ").

% Execute command and send results back to prolog
% this is internally used by tcl_event/3

core("proc prolog_one_event {a_term} {").
core(" global event_socket").
core(" global term_socket").
%% new global variable prolog_variable, wich contain the result of the unification
core(" global prolog_variables").
core("   set result 0").
core("   set result_var 0 ").
core("   puts  $event_socket $a_term.").
core("   flush $event_socket").
core("   gets  $term_socket result").
core("   set ret [unify_term $a_term $result]").
%core("   gets  $event_socket $result_var").
%core("   puts $result_var").
core("} ").

core("proc convert_variable {var} {").
core("   set result \" \" ").
core("   while (1) { ").
core("      set long [string length $var] ").
core("      set pos [string first \"\\""\" $var] ").
core("      if { $pos == -1 } { ").
core("           set result ${result}${var} ").
core("           return $result } ").
core("      incr pos -1 ").
core("      set new [string range $var 0 $pos] ").
core("      incr pos ").
core("      incr pos ").
core("      set var [string range $var $pos $long] ").
core("      set result ${result}${new}'' ").
core("  } ").
core("} ").

% the value of the unification have to be introduced in the global array 
% prolog_variables with the variable name as index
core("proc unify_term {term result} {").
core(" global prolog_variables").
%core("   puts $term ").
%core("   puts $result ").
core("   set long [string length $term] ").
core("   incr long -3 ").
core("   set term [string range $term 8 $long] ").
% the term without execute
core("   set ret [unify_term_aux $term $result ]").
core("   if { $ret == 0 } { ").
core("        return 0 } ").
core("   else { return 1 } ").
core("} ").

core("proc unify_term_aux {term result} {").
core(" global prolog_variables").
core("   set pos_t [string first \"(\" $term] ").
core("   set pos_r [string first \"(\" $result] ").
% the name of the predicate have to be the same 
core("   if { $pos_t == $pos_r } { ").
core("        incr pos_t 1  ").
core("        incr pos_r 1  ").
core("        set long_t [string length $term] ").
core("        set long_r [string length $result] ").
core("        set term_aux [string range $term $pos_t $long_t] ").
core("        set result_aux [string range $result $pos_r $long_r] ").
core("        set long_t [string length $term_aux] ").
core("        set long_r [string length $result_aux] ").
core("        while {$long_t !=  0} { ").
core("             set long_t_1 [string first \",\" $term_aux] ").
core("             set long_r_1 [string first \",\" $result_aux] ").
core("             if { $long_t_1 == -1 || $long_r_1 == -1} { ").
core("                  set long_t_1 [string first \")\" $term_aux] ").
core("                  set long_r_1 [string first \")\" $result_aux] ").
core("             } ").
core("             incr long_t_1 -1").
core("             incr long_r_1 -1").
core("             set term_aux_1 [string range $term_aux 0 $long_t_1] ").
core("             set result_aux_1 [string range $result_aux 0 $long_r_1] ").
core("             set prolog_variables($term_aux_1) $result_aux_1 ").
core("             incr long_t_1 2  ").
core("             incr long_r_1 2  ").
core("             if {$long_t <= $long_t_1 || $long_r <= $long_r_1 } { ").
%core(" puts $prolog_variables(Outputval)").
core("                  return 0 }").
core("             set term_aux [string range $term_aux $long_t_1 $long_t] ").
core("             set result_aux [string range $result_aux $long_r_1 $long_r] ").
core("             set long_t [string length $term_aux] ").
core("             set long_r [string length $result_aux] ").
core("        } ").
core("   } ").
core("} ").


:- comment(version_maintenance,dir('../../version')).

:- comment(version(0*9+79,1999/05/04,20:18*41+'MEST'), "module first
   created (Montse Urraca)").

