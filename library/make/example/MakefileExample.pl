
:- module(_,_,['/home/clip/Systems/ciao/library/make/make',functions]).
%% :- include('/home/clip/Systems/ciao/library/make/make').
%% :- use_package(functions).
:- use_module(library('make/system_extra')).
%:- use_module('../system_extra').
:- use_module(library(lists),[append/3]).
:- use_module(library(terms),[atom_concat/2]).

%% A simple target. Defines the commands necessary to produce the 
%% file 'datafile_simple'.

datafile_simple <-  []    :-
	display('Creating datafile_simple'),nl,
 	writef("Hello world", datafile_simple),
	pause(2).

%% A target with a . Defines the commands necessary to produce the 
%% file 'datafile_simple'.

datafile_double <- [datafile_simple] :-
	display('Creating datafile_double'),nl,
	readf(datafile_simple,Content),
	append(Content,[0'\n|Content],DoubleContent),
 	writef(DoubleContent,datafile_double).

double <= simple :: Name :-
	atom_concat([Name,'.double'],Target),
	display('Creating '),display(Target),nl,
	readf(~atom_concat([Name,'.simple']),Content),
	append(Content,[0'\n|Content],DoubleContent),
 	writef(DoubleContent,Target).

clean <-    :-
	delete_files(~ls('*~|*.asr|*.itf|*.po|*simple|*double')).


the_data([1,2,3]).


%% This is a target which depends on independent_target
dependent_target <- independent_target :-
	display('Doing dependent_target'), nl.

%% texic <= pl :: FileBase :- hello.

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


