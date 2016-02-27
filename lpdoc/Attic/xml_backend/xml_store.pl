:- module(xml_store, [], []).

:- use_module(library(aggregates), [findall/3]).
%:- use_module(library(format), [format_to_string/3]).
:- use_module(fastformat, [format_to_string/3]).


% ======================================================================

:- data xml_content/2.
:- data xml_pred/3.
:- data xml_pred_comment/1.
:- data xml_pred_props/1.
:- data xml_pred_descr/1.
:- data xml_pred_head/1.
:- data xml_pred_prop_acc/1.
:- data xml_site_heading/1.
:- data xml_prop_group/1.

:- export([xml_content/2, xml_pred/3, xml_pred_comment/1,
		xml_pred_props/1, xml_pred_descr/1, xml_pred_head/1,
		xml_pred_prop_acc/1, xml_site_heading/1, xml_prop_group/1]).

% ======================================================================

:- export(module_store/2).
module_store(Section, Content) :-
	assertz_fact(xml_content(Section, Content)).

% ----------------------------------------------------------------------

:- export(module_elem/4).
module_elem(Section, Item, Attribs, Content) :-
	module_store(Section, env(Item, Attribs, Content)).

% ----------------------------------------------------------------------

:- export(module_elem_each/3).
module_elem_each(_,       _,    []).
module_elem_each(Section, Item, [Content|Rest]) :-
	( Content=[] -> T=[]
	; atomic(Content) -> format_to_string("~w", [Content], T)
	; T= Content
	),
	module_elem(Section, Item, [], [T]),
	module_elem_each(Section, Item, Rest).

% ----------------------------------------------------------------------

:- export(module_collect_all/1).
module_collect_all(env('module-documentation', [], Contents)) :-
	findall(Section-Item, xml_content(Section, Item), All),
	module_collect_1(All, Contents).

module_collect_1([],                  []).
module_collect_1([Section-Item|Rest], [ENV|Result]) :-
	module_collect_2(Section, Rest, SecContents, Rest2),
	ENV= env(Section, [], [Item|SecContents]),
	module_collect_1(Rest2, Result).

module_collect_2(_,       [],                  [],                  []).
module_collect_2(Section, [Section-Item|Rest], [Item|RestContents], Rest2) :-
	!, module_collect_2(Section, Rest, RestContents, Rest2).
module_collect_2(_, Rest, [], Rest).


