:- module(io, [current_input/1, current_output/1,
               current_error/1, %%%% pts %%%%
	       set_input/1, set_output/1,
	       set_error/1, %%%% pts %%%%
	       open/3, open/4,
	       close/1, close/2,
	       flush_output/0, flush_output/1,
	       stream_property/2,
	       at_end_of_stream/0, at_end_of_stream/1,
	       set_stream_position/2]).

:- use_module(database, [retractall/1]).
:- use_module(execution, [call_cleanup_det/2]).
:- use_module(err, [err_check/2]).
:- use_module(lists, [member/2]).
:- use_module(char_io, [peekc/1]).

% '$stream_properties'(SN, PastEof, [InOut, TextBinay, EofAction, 
%                                   Repos, Istty, Public | PropList]).

% ----------------
stream_to_num('$stream'(SN), SN) :- !.
stream_to_num(Alias, SN) :-
        clause('$stream_alias'(Alias, SN), _), !.

stream_type(SN, IO, TB) :-
	clause('$stream_properties'(SN, _, [IO,TB|_]), _).

% ----------------
current_input('$stream'(SN)) :-
	'$get_bb'('$current_input', SN), !.
current_input('$stream'(SN)) :-
	nonvar(SN), 
	clause('$stream_properties'(SN, _, _), _), !, fail.
current_input(Stream) :-
	err_check(current_input(Stream), [stream(Stream)]).

current_input_num(SN) :-
	'$get_bb'('$current_input', SN).

% ----------------
current_output('$stream'(SN)) :- 
	'$get_bb'('$current_output', SN), !.
current_output('$stream'(SN)) :-
	nonvar(SN), 
	clause('$stream_properties'(SN, _, _), _), !, fail.
current_output(Stream) :-
	err_check(current_output(Stream), [stream(Stream)]).

current_output_num(SN) :-
	'$get_bb'('$current_output', SN).

% ----------------
check_input_text :-
	'$get_bb'('$current_input_type', text).

check_input_binary :-
	'$get_bb'('$current_input_type', binary).

check_output_text :-
	'$get_bb'('$current_output_type', text).

check_output_binary :-
	'$get_bb'('$current_output_type', binary).

check_error_text :- %%%% pts %%%%
	'$get_bb'('$current_error_type', text).

check_error_binary :- %%%% pts %%%%
	'$get_bb'('$current_error_type', binary).

% ----------------
set_input(Stream) :-
	set_input_type(Stream, Type, set_input(Stream)),
	'$set_bb'('$current_input_type', Type).

% ----------------
set_input_type(Stream, Type, _) :-
	nonvar(Stream),
        stream_to_num(Stream, SN),
	stream_type(SN, input, Type), !,
	'$set_bb'('$current_input', SN).
set_input_type(Stream, Type, Call) :-
	err_check(Call,
		   [inst(Stream),
		    s_or_a(Stream),
		    s_exist(Stream),
		    s_in_out(input, Stream),
		    s_text_binary(input, Type, Stream)
		   ]).

% ----------------
set_output(Stream) :-
	set_output_type('$current_output', Stream, Type, set_output(Stream)),
	'$set_bb'('$current_output_type', Type).

% ----------------
set_output_type(BBName, Stream, Type, _) :- %%%% pts %%%% Dat: added BBName
	nonvar(Stream),
        stream_to_num(Stream, SN),
	stream_type(SN, output, Type), !,
	'$set_bb'(BBName, SN).
set_output_type(_BBName, Stream, Type, Call) :-
	err_check(Call,
		   [inst(Stream),
		    s_or_a(Stream),
		    s_exist(Stream),
		    s_in_out(output, Stream),
		    s_text_binary(output, Type, Stream)
		   ]).

%%%% pts %%%%
set_error(Stream) :-
	set_output_type('$current_error', Stream, Type, set_error(Stream)),
	'$set_bb'('$current_error_type', Type).

% ----------------

open(File, Mode, Stream) :-
	open(File, Mode, Stream, []).

open(File, Mode, Stream, Options) :-
	var(Stream),
	nonvar(Options),
	get_type(Mode, Options, IO, prop(TB, Repos, Eof0)),
	'$open'(File, Mode, SN),
	check_open_err(SN, File), 
	'$stream_type'(SN, CanSeek, Istty),
	check_repos(CanSeek, Repos, SN, open(File, Mode, Stream, Options)), !,
	check_eof_action(Eof0, Istty, Eof),
	Props = [IO, TB, Eof, CanSeek, Istty, public,
                 file_name(File), mode(Mode)],
	asserta('$stream_properties'(SN, false, Props)),
	add_aliases(Options, SN),
	Stream = '$stream'(SN).
open(File, Mode, Stream, Options) :-
	err_check(open(File, Mode, Stream, Options),
		   [var(Stream),
	            inst(File),
		    source_sink(File),
		    inst(Mode),
		    atom(Mode),
		    inst(Options),
		    list(Options),
		    instlist(Options),
		    instlistelements(Options)
		   ]).

check_open_err(SN, _) :- integer(SN), !.
check_open_err(eexist, File) :-
	throw(error(existence_error(source_sink, File), open/4)).
check_open_err(eperm, File) :-
	throw(error(permission_error(open, source_sink, File), open/4)).
check_open_err(_, File) :-
	throw(error(system_error, open(File, '...'))).

check_repos(false, true, SN, Call) :- !,
	'$close'(SN),
	throw(error(permission_error(open, source_sink, reposition(true)), 
	            Call)).
check_repos(_, _, _, _).

check_eof_action(undefined, Istty, Eof) :- !,
	default_eof_action(Istty, Eof).
check_eof_action(Eof, _, Eof).

default_eof_action(true, reset).
default_eof_action(false, eof_code).

get_type(Mode, Options, IO, Prop) :-
	atom(Mode),
	get_io_type(Mode, IO),
	get_properties(Options, prop(text, false, undefined), Prop).

get_io_type(read, input) :- !.
get_io_type(write, output) :- !.
get_io_type(append, output) :- !.
get_io_type(Mode, _) :-
	throw(error(domain_error(io_mode, Mode), open)).

get_properties([], Prop, Prop).
get_properties([Opt|Options], Prop0, Prop) :-
	nonvar(Opt),
	nonvar(Options),
	opt_property(Opt, Prop0, Prop1),
	get_properties(Options, Prop1, Prop).

opt_property(type(TB), prop(_, Repos, Eof), prop(TB, Repos, Eof)) :-
	nonvar(TB),
	(TB = text ; TB = binary), !.
opt_property(reposition(Repos), prop(TB, _, Eof), prop(TB, Repos, Eof)) :-
	nonvar(Repos),
	(Repos = true ; Repos = false), !.
opt_property(eof_action(Eof), prop(TB, Repos, _), prop(TB, Repos, Eof)) :-
	nonvar(Eof),
	(Eof = error ; Eof = eof_code ; Eof = reset), !.
opt_property(alias(Alias), Prop, Prop) :-
	nonvar(Alias),
	nonexistent_alias(Alias), !.
opt_property(E, _, _) :-
	throw(error(domain_error(stream_option, E), open/4)).

nonexistent_alias(Alias) :-
        clause('$stream_alias'(Alias, _), _),
	throw(error(permission_error(open, source_sink, alias(Alias)),
		     open/4)).
nonexistent_alias(_).

add_aliases([], _).
add_aliases([alias(Alias)|Options], SN) :-
	!, 
	asserta('$stream_alias'(Alias, SN)),
	add_aliases(Options, SN).
add_aliases([_|Options], SN) :-
	add_aliases(Options, SN).

% ----------------
close(Stream) :-
	close(Stream, []).

close(Stream, Opts) :-
	nonvar(Stream),
	nonvar(Opts),
        stream_to_num(Stream, SN),
	clause('$stream_properties'(SN, _, [IO|_]), _),
	close_options(Opts), !,
	close_i(Stream, SN, IO).
close(Stream, Opts) :-
	err_check(close(Stream, Opts),
		   [inst(Stream),
		    s_or_a(Stream),
		    s_exist(Stream),
		    inst(Opts),
		    list(Opts),
		    instlist(Opts),
		    instlistelements(Opts)
		   ]).

close_options([]).
close_options([Opt|Opts]) :-
	nonvar(Opt),
	nonvar(Opts),
	close_option(Opt),
	close_options(Opts).

close_option(force(Bool)) :-
	nonvar(Bool),
	(Bool = true ; Bool = false), !.
close_option(Opt) :-
	throw(error(domain_error(close_option, Opt), close/2)).


close_i(_Stream, SN, IO) :-
	(    IO = output -> '$flush'(SN)
	;    true
	),
	\+ clause('$stream_alias'(user_input, SN), _),
	\+ clause('$stream_alias'(user_error, SN), _), %%%% pts %%%%
	\+ clause('$stream_alias'(user_output, SN), _), !,
	(   current_input_num(SN) -> set_input(user_input)
	;   current_output_num(SN) -> set_output(user_output)
	;   current_error_num(SN) -> set_error(user_error)
	;    true
	),
	'$close'(SN),
        retractall('$stream_alias'(_, SN)),
	retractall('$stream_properties'(SN, _, _)).
close_i(_, _, _).


% ----------------
stream_property(Stream0, Property) :-
	(   nonvar(Stream0), stream_to_num(Stream0, SN) -> %%%% pts %%%% Dat: handle aliases !! test case for this
	    Stream='$stream'(SN),
	    check_stream(Stream)
	;   Stream=Stream0,
	    % !! is var(Stream0) standards-compliant?? -- it works in SICStus and SWI-Prolog
	    clause('$stream_properties'(SN, _, _), _), % Dat: may succeed multiple times
	    Stream='$stream'(SN)
	),
	check_property(Property), !,
	findall(Stream-Property, get_stream_property(Stream, Property), Props),
	member(Stream-Property, Props).
stream_property(Stream, Property) :-
	err_check(stream_property(Stream, Property),
	          [stream(Stream),
		   stream_property(Property)]).

check_stream(Stream) :-
	var(Stream), !.
check_stream('$stream'(SN)) :-
	nonvar(SN),
	clause('$stream_properties'(SN, _, _), _).

check_property(Property)         :- var(Property), !.
check_property(type(_))          :- !.
check_property(mode(_))          :- !.
check_property(alias(_))         :- !.
check_property(input)            :- !.
check_property(output)           :- !.
check_property(position(_))      :- !.
check_property(file_name(_))     :- !.
check_property(end_of_stream(_)) :- !.
check_property(eof_action(_))    :- !.
check_property(reposition(_))    :- !.

get_stream_property('$stream'(SN), Property) :-
	( var(SN) -> Public = public ; true ), 
	clause('$stream_properties'(SN, PastEof,
                                    [IO,TB,Eof,Repos,Istty,Public|Props]), _),
	(   Property = IO
	;   Property = type(TB)
	;   Property = reposition(Repos)
	;   Property = eof_action(Eof)
	;   member(Property, Props)
	;   Property = alias(Alias), clause('$stream_alias'(Alias, SN), _)
	;   Property = end_of_stream(E), 
               get_end_of_stream(IO, SN, PastEof, Istty, E)
	;   Repos = true ->
	       Property = position('$stream_position'(Pos)),
	       '$get_pos'(SN, Pos)
	).

get_end_of_stream(output, _, _, _, not) :- !.
get_end_of_stream(input, _, _, true, not) :- !.
get_end_of_stream(input, _, true, _, past) :- !.
get_end_of_stream(input, SN, _, _, at) :- 
	with_input('$stream'(SN), _, peekc(C0), get_end_of_stream),
	C0 = -1, !.
get_end_of_stream(input, _, _, _, not).
	

% ----------------
at_end_of_stream :-
	current_input(S),
	at_end_of_stream(S).

at_end_of_stream(Stream) :-
	nonvar(Stream),
	stream_to_num(Stream, SN),
	clause('$stream_properties'(SN, PastEof, [input,_,_,_,Istty|_]), _), !,
	get_end_of_stream(input, SN, PastEof, Istty, E), E \= not.
at_end_of_stream(Stream) :-
	err_check(at_end_of_stream(Stream),
		   [inst(Stream),
		    s_or_a(Stream),
		    s_exist(Stream),
		    s_in_out(input, Stream)
		   ]).

% ----------------
set_stream_position(Stream, Pos) :-
	nonvar(Stream),
	nonvar(Pos),
	stream_to_num(Stream, SN),
	check_stream_position(Pos, PosNum),
	Props = [_,_,_,true|_],
	retract('$stream_properties'(SN, _, Props)), !,
	asserta('$stream_properties'(SN, false, Props)),
	'$set_pos'(SN, PosNum).
set_stream_position(Stream, Pos) :-
	err_check(set_stream_position(Stream, Pos), 
	          [inst(Stream),
		   inst(Pos),
		   s_or_a(Stream),
		   position(Pos),
		   s_exist(Stream),
		   s_reposition(Stream)]).

check_stream_position('$stream_position'(PosNum), PosNum).
	
% ----------------
init_streams :-
	current_input_num(StdIn),
	current_output_num(StdOut),
	error_stream('$stream'(StdErr)),
	'$stream_type'(StdIn, Inseek, Intty),
	'$stream_type'(StdOut, Outseek, Outtty),
	'$stream_type'(StdErr, Errseek, Errtty),
	default_eof_action(Intty, InEof),
	default_eof_action(Outtty, OutEof),
	default_eof_action(Errtty, ErrEof),
        asserta('$stream_alias'(user_input, StdIn)),
        asserta('$stream_alias'(user_output, StdOut)),
        asserta('$stream_alias'(user_error, StdErr)), %%%% pts %%%%
	asserta('$stream_properties'(StdIn, false, [input,text,InEof,
	                             Inseek, Intty, public,mode(read)])),
	asserta('$stream_properties'(StdOut, false, [output,text,OutEof,
	                             Outseek,Outtty,public,mode(append)])),
	asserta('$stream_properties'(StdErr, false, [output,text,ErrEof,
	                             Errseek,Errtty,private,mode(append)])),
	'$set_bb'('$current_input_type', text),
	'$set_bb'('$current_output_type', text),
	'$set_bb'('$current_error_type', text). %%%% pts %%%%

% ----------------
with_input(Stream, Type, Goal, Call) :-
	current_input(CI),
	set_input_type(Stream, Type, Call),
	call_cleanup_det(Goal, set_input(CI)).

% ----------------
with_output(Stream, Type, Goal, Call) :-
	current_output(CO),
	set_output_type('$current_output', Stream, Type, Call),
	call_cleanup_det(Goal, set_output(CO)).

% ----------------
with_error(Stream, Type, Goal, Call) :- %%%% pts %%%%
	current_error(CO),
	set_output_type('$current_error', Stream, Type, Call),
	call_cleanup_det(Goal, set_error(CO)).

% ----------------
restore_streams(CurrIn, CurrOut, CurrErr) :-
	set_input(CurrIn),
	set_output(CurrOut),
	set_error(CurrErr).

default_streams(Goal) :-
	current_input(CurrIn),
	current_output(CurrOut),
	current_error(CurrErr), %%%% pts %%%%
	set_input(user_input),
	set_output(user_output),
	set_error(user_error),
	call_cleanup_det(Goal, restore_streams(CurrIn,CurrOut,CurrErr)).

% ----------------
error_stream('$stream'(S)) :-
	'$get_bb'('$current_error', S).

%%%% pts %%%%
current_error('$stream'(SN)) :- 
	'$get_bb'('$current_error', SN), !.
current_error('$stream'(SN)) :-
	nonvar(SN), 
	clause('$stream_properties'(SN, _, _), _), !, fail.
current_error(Stream) :-
	err_check(current_error(Stream), [stream(Stream)]).

%%%% pts %%%%
current_error_num(SN) :-
	'$get_bb'('$current_error', SN).

% ----------------
flush_output :-
	current_output_num(SN),
	'$flush'(SN).

flush_output(Stream) :-
	nonvar(Stream),
	stream_to_num(Stream, SN),
	stream_type(SN, output, _), !, 
	'$flush'(SN).
flush_output(Stream) :-
	err_check(flush_output(Stream),
		   [inst(Stream),
		    s_or_a(Stream),
		    s_exist(Stream),
		    s_in_out(output, Stream)
		   ]).

% ----------------
handle_eof(Action) :-
	current_input_num(SN),
	clause('$stream_properties'(SN, PastEof, [_,_,Eof|_]), _),
	handle_eof(Eof, PastEof, Action, SN).

handle_eof(eof_code, false, get, SN) :- !,
	set_past_eof(SN).
handle_eof(error, false, get, SN) :- !,
	set_past_eof(SN).
handle_eof(error, true, _, SN) :- !,
	throw(error(permission_error(input, past_end_of_stream, '$stream'(SN)),
	            getc)).
handle_eof(_, _, _, _).

set_past_eof(SN) :-
	retract('$stream_properties'(SN, _, Props)),
	asserta('$stream_properties'(SN, true, Props)).

% ----------------
read_line([], 10) :- !.
read_line([], -1) :- !.
read_line([X|L], _) :-
	get_code(X),
	read_line(L, X).

read_line(L) :-
	read_line(L, 0).

