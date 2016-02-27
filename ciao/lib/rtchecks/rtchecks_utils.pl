:- module(rtchecks_utils, [handle_rtcheck/1, call_rtc/1, pretty_messages/1,
		rtcheck_to_messages/3, save_rtchecks/1, load_rtchecks/1],
	    [assertions, nativeprops, regtypes, hiord]).

:- use_module(library(write), []).
:- use_module(library(hiordlib)).
:- use_module(library(lists)).
:- use_module(library(rtchecks(compact_list))).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains some useful predicates to
	facilitate work with run-time checks.").

:- doc(handle_rtcheck/1, "Predicate that processes a rtcheck exception.").

:- regtype rtcheck_error/1 #
	"Specifies the format of a run-time check exception.".

rtcheck_error(rtcheck(Type, _Pred, _Prop, _Valid, Locs)) :-
	rtcheck_type(Type),
	list(Locs).

:- regtype rtcheck_type/1 # "Specifies the type of run-time errors.".

rtcheck_type(comp).
rtcheck_type(pp_check).
rtcheck_type(success).
rtcheck_type(compat).
rtcheck_type(compatpos).
rtcheck_type(calls).

pretty_messages(Messages) :-
	push_prolog_flag(write_strings, on),
	compact_list(Messages, Messages1),
	messages(Messages1),
	fail.
pretty_messages(_) :-
	pop_prolog_flag(write_strings).

:- pred handle_rtcheck/1 : rtcheck_error.

handle_rtcheck(RTCheck) :-
	rtcheck_to_messages(RTCheck, Messages, []),
	pretty_messages(Messages).

position_to_message(predloc(Pred, loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error,
		['Failed in ', ''(Pred), '.'])).
position_to_message(callloc(Pred, loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error,
		['Failed during invocation of ', ''(Pred)])).
position_to_message(litloc(Lit, loc(S, Ln0, Ln1)-(Pred)),
	    message_lns(S, Ln0, Ln1, error,
		['Failed when invocation of ', ''(Pred),
		    ' called ', ''(Lit), ' in its body.'])).
position_to_message(asrloc(loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error, [])).
position_to_message(pploc(loc(S, Ln0, Ln1)),
	    message_lns(S, Ln0, Ln1, error, [])).

:- pred rtcheck_to_messages(RTCheck, Messages0, Messages)
	:: (list(Messages0, message_info), list(Messages, message_info))
	: rtcheck_error(RTCheck) + is_det # "Converts a run-time error
	in a message or a list of messages. @var{Messages} is the tail.".

rtcheck_to_messages(rtcheck(Type, Pred, Prop, Valid, Positions),
	    Messages0, Messages) :-
	map(Positions, position_to_message, PosMessages0),
	reverse(PosMessages0, PosMessages),
	Text = ['Run-time check failure in assertion for:\n\t',
	    ''(Pred),
	    '.\nIn *', Type, '*, unsatisfied property: \n\t',
	    ''(Prop), '.'|Text0],
	( Valid = [] -> Text0 = Text1
	; Text0 = ['\nBecause: \n\t', ''(Valid), '.'|Text1]
	),
	(
	    select(message_lns(S, Ln0, Ln1, MessageType, Text2),
		PosMessages, PosMessages1) ->
	    (Text2 == [] -> Text1 = [] ; Text1 = [' '|Text2]),
	    Message = message_lns(S, Ln0, Ln1, MessageType, Text)
	;
	    Text1 = [],
	    Message = message(error, Text),
	    PosMessages1 = PosMessages
	),
	append([Message|PosMessages1], Messages, Messages0).

:- meta_predicate call_rtc(goal).

:- pred call_rtc/1 : callable # "This predicate calls a goal and if a rtcheck
	exception is intercepted, an error message is shown and the
	execution continues.".

call_rtc(Goal) :-
	intercept(Goal, rtcheck(Type, Pred, Prop, Valid, Poss),
	    handle_rtcheck(rtcheck(Type, Pred, Prop, Valid, Poss))).

:- use_module(library(aggregates), [findall/3]).

:- data rtcheck_db/1.


:- meta_predicate save_rtchecks(goal).

:- pred save_rtchecks/1 : callable # "Asserts in rtcheck_db/1 all the
	run-time check exceptions thrown by the goal.".

save_rtchecks(Goal) :-
	retractall_fact(rtcheck_db(_)),
	intercept(Goal, rtcheck(Type, Pred, Prop, Valid, Poss),
	    assertz_fact(rtcheck_db(rtcheck(Type, Pred, Prop, Valid, Poss)))).

:- pred load_rtchecks/1 => list(rtcheck_error) # "retract the
	rtcheck_db/1 facts and return them in a list.".

load_rtchecks(RTChecks) :-
	findall(RTCheck, retract_fact(rtcheck_db(RTCheck)), RTChecks).
