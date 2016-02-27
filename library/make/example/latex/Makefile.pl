
:- module(_,_,['/home/herme/lpmake/make/make',functions]).
%% :- include('/home/herme/lpmake/make/make').
%% :- use_package(functions).
%% :- use_module(library('make/system_extra')).
:- use_module('/home/herme/lpmake/make/system_extra').
:- use_module(library(terms),[atom_concat/2]).

%% This is what make calls to report processing simple targets
%% dependency_comment(SSuffix,TSuffix,FileBase) :- 
%% 	message(['Generating ',FileBase,'.',TSuffix,
%%                  ' from ',FileBase,'.',SSuffix]).
target_comment(Target) :-
 	message(['Processing ',Target,'.']).

%% dvi <= tex :: File :-
%% 	system(~atom_concat(['latex ',File,'.tex'])).

dvi <= tex :: File :-
	atom_concat(['latex ',File,'.tex'],Command),
	system(Command).

ps  <= dvi :: File :-
	system(~atom_concat(['dvips -o ',File,'.ps ',File,'.dvi'])).

clean <- :-
	delete_files(~ls('*aux|*log|*~|*.asr|*.itf|*.po')).

%% -------------------------------------------------------------------------
%% %% This is what make calls for simple targets
%% target_comment(Target) :-
%% 	message(['Processing ',Target,'.']).
%% 	
%% %% This is what make calls for dependency targets
%% dependency_comment(SSuffix,TSuffix,FileBase) :- 
%% 	message(['Generating ',FileBase,'.',TSuffix,
%%                  ' from ',FileBase,'.',SSuffix]).
%% 
%% %% This is what the top level calls when printing the usage message
%% target_comment_action(Target, Comment) :- 
%% 	list_concat(["Generate ",Target],Comment).
%% -------------------------------------------------------------------------


