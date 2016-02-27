:- module(_, [pbundles_to_wipe/4], [fsyntax, hiord]).

:- use_module(library(aggregates)).
:- use_module(library(hiordlib)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(messages)).
:- use_module(library(sort)).
:- use_module(library(terms)).

:- use_module(library(system_extra)).

:- use_module(library(lpdist(pbundle_meta))).

% TODO: Code does not seem to do what documentation says:
%       (see revision_rule/2)
%
% To save space, the release of the day only preserves the
% distributions generated the last 15 days, the monthly distributions
% the last year, etc. 

% ---------------------------------------------------------------------------

% PBundlesDir: the directory where pbundles are stored locally
% Keep: the list of stable revisions that will not be wiped.
% Removable: the revisions that can be removed
pbundles_to_wipe(PBundlesDir, Branch, Keep, Removable) :-
 	Xs0 = ~load_pbundle_metas(Branch, PBundlesDir),
	Xs = ~reverse(~sort_pbundle_metas_by_timestamp(Xs0)),
	unwanted(Xs, Keep, Removable).

% ---------------------------------------------------------------------------

:- use_module(library(lpdist(datetime))).

% unwanted(Xs, Keep, Ys):
%   Ys are the names of pbundles in Xs, minus Keep, that are outdated.
unwanted([], _, []).
unwanted(Xs, Keep, Ys) :-
	% Obtain the time of first and the last pbundles
	Xs = [First0|_], append(_, [Last0], Xs),
	First = ~pbundle_meta_time(First0),
	Last = ~pbundle_meta_time(Last0),
	%
	unwanted_(Xs, First, Last, Keep, Ys).

unwanted_([], _, _, _, []).
unwanted_([X|Xs], First, Last, Keep, Ys) :-
	( ( First = Last % TODO: ad-hoc, check up_to_date
	  ; Time = ~pbundle_meta_time(X),
	    up_to_date(Time, First, Last)
	  ; member(Name, Keep),
	    % TODO: Name must be an atom at this moment...
	    pbundle_meta_has_name(X, Name)
	  ) ->
	    Ys = Ys0
	; Ys = [BaseDir|Ys0],
	  BaseDir = ~pbundle_meta_attr(X, basedir)
	),
	unwanted_(Xs, First, Last, Keep, Ys0).

pbundle_meta_time(PMeta) := Time :-	
	PackDate = ~pbundle_meta_attr(PMeta, commit_date),
	date_iso8601_to_timestamp(PackDate, Timestamp),
	Time is Timestamp // 86400. % seconds in a day

% TODO: Review this code, it seems strange... --JFMC
up_to_date(Time, First, Last) :-
	revision_rule(Period, HistorySize),
	Last - Time < HistorySize,
	Time - First >= Period,
	!.

% revision_rule(Period, HistorySize)
revision_rule(180, 0x7FFFFFFF). % Infinite
revision_rule(90, 732).
revision_rule(30, 366).
revision_rule(1, 15).
%revision_rule(0, 1). % TODO: I added this one to keep the latest version

