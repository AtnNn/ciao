:- module(messages,[
	    error_message/1,
	    error_message/2,
	    error_message/3,
	    warning_message/1,
	    warning_message/2,
	    warning_message/3,
 	    note_message/1,
	    note_message/2,
	    note_message/3,
 	    simple_message/1,
	    simple_message/2,
	    optional_message/2,
	    optional_message/3,
 	    debug_message/1,
	    debug_message/2,
 	    debug_goal/2,
 	    debug_goal/3
	],
        [
            assertions,regtypes,isomodes
        ]).

%% NOTE: if you change the output format of messages you 
%%       will probably also want to change ciao.el

:- use_module(library(format),[format/3,format_control/1]).

% Other libraries
:- use_module(library(lists)).
:- use_module(library(filenames),[no_path_file_name/2]).
:- use_module( library( strings ) , [write_string/1] ).

:- set_prolog_flag(multi_arity_warnings, off).

%% ---------------------------------------------------------------------------

:- comment(title,"Printing status and error messages").

:- comment(author,"The CLIP Group").

:- comment(module,"This is a very simple library for printing status
     and error messages to the console.").

:- comment(bug, "Debug message switching should really be done with an
   expansion, for performance.").

:- comment(doinclude,location/1).
:- regtype location/1 # "Identifies a program source line.".

location(loc(File,L1,L2)):- atm(File), int(L1), int(L2).

%% ---------------------------------------------------------------------------

:- pred error_message(Text) : string 
   # "The text provided in @var{Text} is printed as an ERROR message.".

:- impl_defined(error_message/1).
:- meta_predicate error_message(addmodule).

error_message(Message,Module) :-
	compose("ERROR",Module,Message).

:- pred error_message(Text,ArgList) : format_control * list 
   # "The text provided in @var{Text} is printed as an ERROR message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}.".

:- impl_defined(error_message/2).
:- meta_predicate error_message(?,addmodule).

error_message(Message,A,Module) :-
	compose("ERROR",Module,Message,A).

:- pred error_message(Lc,Text,ArgList) : location * format_control * list 
   # "The text provided in @var{Text} is printed as an ERROR message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}, and
     reporting error location @var{Lc} (file and line numbers).".

:- impl_defined(error_message/3).
:- meta_predicate error_message(?,?,addmodule).

error_message(Loc,Message,A,Module) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	compose("ERROR",Module,File,LB,LE,Message,A).
error_message(_Loc,Message,A,Module) :-
	compose("ERROR",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred warning_message(Text) : string 
   # "The text provided in @var{Text} is printed as a WARNING message.".

:- impl_defined(warning_message/1).
:- meta_predicate warning_message(addmodule).

warning_message(Message,Module) :-
	compose("WARNING",Module,Message).

:- pred warning_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a WARNING message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}.".

:- impl_defined(warning_message/2).
:- meta_predicate warning_message(?,addmodule).

warning_message(Message,A,Module) :-
	compose("WARNING",Module,Message,A).

:- pred warning_message(Lc,Text,ArgList) : location * format_control * list 

   # "The text provided in @var{Text} is printed as a WARNING message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}, and
     reporting error location @var{Lc} (file and line numbers).".

:- impl_defined(warning_message/3).
:- meta_predicate warning_message(?,?,addmodule).

warning_message(Loc,Message,A,Module) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	compose("WARNING",Module,File,LB,LE,Message,A).
warning_message(_Loc,Message,A,Module) :-
	compose("WARNING",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred note_message(Text) : string 
   # "The text provided in @var{Text} is printed as a NOTE.".

:- impl_defined(note_message/1).
:- meta_predicate note_message(addmodule).

note_message(Message,Module) :-
	compose("NOTE",Module,Message).

:- pred note_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a NOTE, using the
     arguments in @var{ArgList} to interpret any variable-related
     formatting commands embedded in @var{Text}.".

:- impl_defined(note_message/2).
:- meta_predicate note_message(?,addmodule).

note_message(Message,A,Module) :-
	compose("NOTE",Module,Message,A).

:- pred note_message(Lc,Text,ArgList) : location * format_control * list 

   # "The text provided in @var{Text} is printed as a NOTE, using the
     arguments in @var{ArgList} to interpret any variable-related
     formatting commands embedded in @var{Text}, and reporting error
     location @var{Lc} (file and line numbers).".

- impl_defined(note_message/3).
:- meta_predicate note_message(?,?,addmodule).

note_message(Loc,Message,A,Module) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	compose("NOTE",Module,File,LB,LE,Message,A).
note_message(_Loc,Message,A,Module) :-
	compose("NOTE",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred simple_message(Text) : string 
   # "The text provided in @var{Text} is printed.".

simple_message(Message) :-
	simple_message(Message,[]).

:- pred simple_message(Text,ArgList) : format_control * list 
   # "The text provided in @var{Text} is printed as a message,
     using the arguments in @var{ArgList}.".

simple_message(Message,A) :-
	append([0'{ | Message],"}\n",NMessage),
	format(user_error,NMessage,A).

%% ---------------------------------------------------------------------------

:- pred optional_message(Text,Opts) : string * list(atm)
   # "The text provided in @var{Text} is printed as a message, but
     only if the atom @tt{-v} is a member of @var{Opts}. These
     predicates are meant to be used for optional messages, which are
     only to be printed when @em{verbose} output is requested
     explicitly.".

optional_message(Message,Opts) :-
	optional_message(Message,[],Opts).

:- pred optional_message(Text,ArgList,Opts) : format_control * list * list(atm)
   # "The text provided in @var{Text} is printed as a message, using
     the arguments in @var{ArgList}, but only if the atom @tt{-v} is a
     member of @var{Opts}. These predicates are meant to be used for
     optional messages, which are only to be printed when @em{verbose}
     output is requested explicitly.".

optional_message(Message,A,Opts) :-
	member('-v',Opts),
	!,
	simple_message(Message,A).
optional_message(_Message,_A,_Opts).

%% ---------------------------------------------------------------------------

:- pred debug_message(Text) : format_control 

   # "The text provided in @var{Text} is printed as a debugging
      message.  These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- impl_defined(debug_message/1).
:- meta_predicate debug_message(addmodule).

debug_message(Message,Module) :-
	(  issue_debug_messages(Module)
	-> compose("DEBUG",Module,Message)
	;  true ).

:- pred debug_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a debugging
      message, using the arguments in @var{ArgList} to interpret any
      variable-related formatting commands embedded in
      @var{Text}. These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} which the module name as
      argument.".

:- impl_defined(debug_message/2).
:- meta_predicate debug_message(?,addmodule).

debug_message(Message,A,Module) :-
	(  issue_debug_messages(Module)
	-> compose("DEBUG",Module,Message,A)
	;  true ).

:- pred issue_debug_messages(Module) => atom

   # "Printing of debugging messages is enabled for module @var{Module}.".

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

%% ---------------------------------------------------------------------------

:- pred debug_goal(Goal,Text) 

   # "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message.  The whole process (including
      running @var{Goal}) is turned @tt{on} by defining a fact of
      @pred{issue_debug_messages/1} with the module name as
      argument.".

:- impl_defined(debug_goal/2).
:- meta_predicate debug_goal(goal,addmodule).

debug_goal(Goal,Message,Module) :-
	(  issue_debug_messages(Module)
	-> call(Goal),
	   compose("DEBUG",Module,Message)
	;  true ).

:- pred debug_goal(Goal,Text,ArgList) 

   # "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message, using the arguments in
      @var{ArgList} to interpret any variable-related formatting
      commands embedded in @var{Text}. Note that the variables in
      @var{ArgList} can be computed by @var{Goal}.  The whole process
      (including running @var{Goal}) is turned @tt{on} by defining a
      fact of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- impl_defined(debug_goal/3).
:- meta_predicate debug_goal(goal,?,addmodule).

debug_goal(Goal,Message,A,Module) :-
	(  issue_debug_messages(Module)
	-> call(Goal),
	   compose("DEBUG",Module,Message,A)
	;  true ).

%% ---------------------------------------------------------------------------

:- pred compose(Type,Module,Mess) 
   : string * atm * string

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, with error message @var{Mess}.".

compose(Type,Module,Mess) :-
	append("{~s (~q): ",Mess,T1),
	append(T1,"}~n",CMess),
%       NEW METHOD
%	sformat( S ,CMess,[Type,Module]),
%	display_long_string( S , user_error ).
	prolog_flag(write_strings, Old, on),
	format(user_error,CMess,[Type,Module]),
	set_prolog_flag(write_strings, Old).


:- pred compose(Type,Module,Mess,Args) 
   : string * atm * format_control * list

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, with error message @var{Mess} containing arguments
      @var{Args}.".

compose(Type,Module,Mess,Args) :-
	append("{~s (~q): ",Mess,T1),
	append(T1,"}~n",CMess),
	simplify_module(Module,SimplifiedModule),
%       NEW METHOD
%	sformat( S , CMess,[Type,SimplifiedModule|Args]),
%	display_long_string( S , user_error ).
	prolog_flag(write_strings, Old, on),
	format(user_error,CMess,[Type,SimplifiedModule|Args]),
	set_prolog_flag(write_strings, Old).


simplify_module(user(Path),SimplifiedModule) :-
	no_path_file_name(Path,SimplifiedModule),
	!.
simplify_module(Path,SimplifiedModule) :-
	no_path_file_name(Path,SimplifiedModule),
	!.
simplify_module(Module,Module).


:- pred compose(Type,Module,File,LB,LE,Mess,Args) 
   : string * atm * atm * int * int * format_control * list

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, while processing file @var{File}, between line
      numbers @var{LB} and @var{LE}, with error message @var{Mess}
      containing arguments @var{Args}.".

compose(Type,Module,File,LB,LE,Mess,Args) :-
	append("{In ~w~n~s (~q): (lns ~w-~w) ",Mess,T1),
	append(T1,"~n}~n",CMess),
%       NEW METHOD
%	sformat( S , CMess,[File,Type,Module,LB,LE|Args]),
%	display_long_string( S , user_error ).
	prolog_flag(write_strings, Old, on),
	format(user_error,CMess,[File,Type,Module,LB,LE|Args]),
	set_prolog_flag(write_strings, Old).


%% ---------------------------------------------------------------------------
:- pred space( N ) : num( N )
# "prints @var{N} spaces.".


space( 0 ) :- !.

space( N ) :-
	N > 0,
	!,
	N1 is N - 1,
	display( ' ' ),
	space( N1 ).

space( _ ).




display_long_string( X , S ) :-
	line_position( user_output , NInit ),
	display_long_string_n( X , S , NInit , [79] ).

display_long_string_n( X , S , I , Max ) :-
	append( WORD_WO_S  , [SEP|XE] , X ),
 	member( SEP , " ,[]()" ),
	!,
	append( WORD_WO_S , [SEP] , WORD ),
	nl_if_necessary( WORD , XE , S , I , Max , Current ),
	display__compute_next_space( WORD , Current , I , FI  ),
	write_string( WORD ),
	display_long_string_n( XE , S , FI , Max ).

display_long_string_n( WORD , S , I , Max ) :-
	nl_if_necessary( WORD , "" , S , I , Max , Current ) , 
	display__compute_next_space( WORD , Current , I ,  _ ),
	write_string( WORD ).




% nil
display__compute_next_space( [] , _ , I , I ) :- 
	!.

% push
display__compute_next_space( [SEP|R] , Current , I , FI ) :-
	member( SEP , "[({" ) ,
	!,
	NCurrent is Current + 1,
        NSpace   is Current + 1,
	display__compute_next_space( R , NCurrent , [ NSpace | I ] , FI ).
	
% pop
display__compute_next_space( [SEP|R] , Current , [ _ | NI ] , FI ) :-
	member( SEP , "])}" ) ,
	!,
	NCurrent is Current + 1,
	display__compute_next_space( R , NCurrent , NI , FI ).

% normal char	
display__compute_next_space( [_|R] , Current , I , FI ) :-
	NCurrent is Current + 1,
	display__compute_next_space( R , NCurrent , I , FI ),
	!.

% if it fails (?)
display__compute_next_space( _ , _ , I , I ).


:- use_module( library( write ) ).

nl_if_necessary( WORD , REST , S , I , Max , C ) :-
	line_position( S , Current ),
	word_length( WORD , REST , Current , Max , WL ) , 
	(
	    WL + Current + 1 > Max
	->
	    nl, 
	    I = [ C | _ ] ,
	    space( C )
	;
	    C = Current
	).

nl_if_necessary( _ , _ , S , _ , C ) :- 
	line_position( S , C ).




% The lenght of a "word" is:
% * if it a list -> sumatory of the length of its members
% * lenght( word )
%
% Optimization: We can stop counting if
%  Current + CurrentComputedLen > Max
word_length( [ 0'[ | WORD ] , REST , Current , Max , WL2 ) :-
	!,
	word__count( WORD , REST , 1 , 0 , Current , Max , WL ),
	WL2 is WL - Current + 1.

word_length( [ 0'( | WORD ] , REST , Current , Max , WL2 ) :-
	!,
	word__count( WORD , REST , 0 , 1 , Current , Max , WL ),
	WL2 is WL - Current + 1.

word_length( WORD , REST , Current , Max , WL2 ) :-
	word__count( WORD , REST , 0 , 0 , Current , Max , WL ),
	WL2 is WL - Current.
%word_length( WORD , _ , _ , _ , WL ) :-
%	length( WORD , WL ).




:- pred word__count( _ , _ , _ , _ , Current , Max , Current ) :
	ground * ground * num * num * num * num * var.

% Start counting till balanced separator is reached
% S = number of [ ] (square parentheris) not yet balanced
% P = number of ( ) (parentheris) not yet balanced
word__count( [] , _ , 0 , 0 , WL , _ , WL ) :-
	!.

word__count( _ , [] , 0 , 0 , WL , _ , WL ) :-
	!.


word__count( _ , _ , _ , _ , Current , Max , WL ) :-
	Current > Max,
	WL = Current,
	!.

word__count( [ 0'[ | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	S1 is S + 1,
	C1 is Current + 1,
	word__count( WORD , REST , S1 , P , C1 , Max , WL ).

word__count( [ 0'( | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	P1 is P + 1,
	C1 is Current + 1,
	word__count( WORD , REST , S , P1 , C1 , Max , WL ).

word__count( [ 0'] | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	S1 is S - 1,
	C1 is Current + 1,
	( 
	    S1 >= 0
	->
	    word__count( WORD , REST , S1 , P , C1 , Max , WL )
	;
	    C1 = WL
	).

word__count( [ 0') | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	P1 is P - 1,
	C1 is Current + 1,
	(
	    P1 >= 0
	->
	    word__count( WORD , REST , S , P1 , C1 , Max , WL )
	;
	    C1 = WL
	).

word__count( [ _ | WORD ] , REST , S , P , Current , Max , WL ) :-
	!,
	C1 is Current + 1,
	word__count( WORD , REST , S , P , C1 , Max , WL ).

word__count( [] , [] , _ , _ , WL , _ , WL ) :-
	!.
	
word__count( [] , REST , S , P , Current , Max , WL ) :-
	!,
	word__count( REST , [] , S , P , Current , Max , WL ).

word__count( [] , _ , 0 , 0 , WL , _ , WL ) :-
	!.

%% ---------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*11+233,2004/05/27,17:43*49+'CEST'), "Used
   sformat/3 to print messages in 83 characters-wide format (David
   Trallero Mena)").

:- comment(version(1*11+185,2004/02/13,15:27*40+'CET'), "Taken out
   metaprops.  (Francisco Bueno Carrillo)").

:- comment(version(1*11+127,2003/12/30,22:01*35+'CET'), "Added comment
   author.  (Edison Mera)").

:- comment(version(1*3+108,1999/11/18,13:48*03+'MET'), "Imported
   @lib{regtypes} package. Still using @tt{^}, though. (Manuel
   Hermenegildo)").

:- comment(version(0*9+59,1999/04/26,14:11*25+'MEST'), "Added optional
   messages (which print only if @tt{-v} is present in a list of
   options.  (Manuel Hermenegildo)").

:- comment(version(0*8+24,1998/12/04,18:43*27+'MET'), "The format of
   error/warning messages which include a file name is now compatible
   with that of the engine(io_aux) library (used by the
   compiler). This means that they are parseable right away by the
   CIAO emacs mode.  (Manuel Hermenegildo)").

:- comment(version(0*8+22,1998/12/03,19:16*36+'MET'), "Now tolerates
   messages with incorrectly formatted locations (even a free var):
   simply print message with no location info.  (Manuel
   Hermenegildo)").

:- comment(version(0*8+21,1998/12/03,14:40*18+'MET'), "Messages
   specifying location now take begin and end line numbers.  (Manuel
   Hermenegildo)").

:- comment(version(0*8+13,1998/12/02,13:55*27+'MET'), "Added
   @pred{debug_goal/2} and @pred{debug_goal/3}. Also, turned on
   @tt{write_strings} flag when printing messages. (Manuel Hermenegildo)").

:- comment(version(0*7+27,1998/11/17,22:22*42+'MET'), "Changed name 
   of exports so that they do not clash with module engine(io_aux).
   (Francisco Bueno Carrillo)").

:- comment(version(0*5+41,1998/07/07,11:18*04+'MET DST'), "Changed
   message format to improve readability when reporting file/line
   number combinations (suggestion from F. Bueno).  (Manuel
   Hermenegildo)").

:- comment(version(0*5+32,1998/06/30,18:22*43+'MET DST'), "Calling
   module name now included automatically in error messages.  (Manuel
   Hermenegildo)").

:- comment(version(0*5+29,1998/06/30,11:29*20+'MET DST'), "Added
   debug/1 and debug/2, plus how to turn them on and off.  (Manuel
   Hermenegildo)").

:- comment(version(0*5+10,1998/05/07,14:19*20+'MET DST'), "Added
   error/3 for line numbers.  (Francisco Bueno Carrillo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

:- comment(version(0*1+2,1997/12/19), "Ported to native CIAO. (Manuel
   Hermenegildo)").

:- comment(version(0*1+1,1997/9/17), "Fixed minor bug in declaration
   of modules used. (Manuel Hermenegildo)").

:- comment(version(0*1+1,1997/7/12), "Started documentation. (Manuel
   Hermenegildo)").

%% ---------------------------------------------------------------------------
